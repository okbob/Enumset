/*
 * contrib/enumset/enumset.c
 */
#include "postgres.h"

#include <limits.h>

#include "catalog/pg_enum.h"
#include "parser/parse_type.h"

#include "utils/builtins.h"
#include "utils/formatting.h"
#include "utils/lsyscache.h"
#include "utils/syscache.h"
#include "nodes/bitmapset.h"
#include "utils/array.h"
#include "catalog/pg_type.h"
#include "fmgr.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

#define BITMAPSET_SIZE(nwords)	\
    (offsetof(Bitmapset, words) + (nwords) * sizeof(bitmapword))

typedef struct
{
	int32	vl_len_;			/* varlena header */
	int32	typmod;
} EnumsetType;

typedef struct Enumset
{
	int32	typmod;					/* it is -1 or typid of selected enum type */
	Bitmapset	*bms;				/* bitmaps of known enums or bitmaps of integers */
	ArrayType	*array;				/* set of unknown enums stored as array */
} Enumset;

typedef struct EnumsetIOData
{
	int32		typmod;
	Oid		typiofunc;
	Oid		typioparam;
	FmgrInfo	proc;		/* lookup result for typiofunc */
} EnumsetIOData;

#define DatumGetEnumsetP(X)		enumset_deserialize((EnumsetType *) PG_DETOAST_DATUM(X))
#define PG_GETARG_ENUMSET_P(X)		DatumGetEnumsetP(PG_GETARG_DATUM(X))
#define DatumGetEnumsetPP(X)		enumset_deserialize((EnumsetType *) PG_DETOAST_DATUM_PACKED(X))
#define PG_GETARG_ENUMSET_PP(X)		DatumGetEnumsetPP(PG_GETARG_DATUM(X))

#define EnumsetPGetDatum(X)		PointerGetDatum(enumset_serialize(X))
#define PG_RETURN_ENUMSET_P(X)		return EnumsetPGetDatum(X)


PG_FUNCTION_INFO_V1(enumsetin);
PG_FUNCTION_INFO_V1(enumsetout);
PG_FUNCTION_INFO_V1(enumset_typmod_in);
PG_FUNCTION_INFO_V1(enumset_typmod_out);
PG_FUNCTION_INFO_V1(enumset_typmod);

Datum enumsetin(PG_FUNCTION_ARGS);
Datum enumsetout(PG_FUNCTION_ARGS);
Datum enumset_typmod_in(PG_FUNCTION_ARGS);
Datum enumset_typmod_out(PG_FUNCTION_ARGS);
Datum enumset_typmod(PG_FUNCTION_ARGS);


/*
 * datum_compute_size() and datum_write() are used to insert the bound
 * values into a range object.  They are modeled after heaptuple.c's
 * heap_compute_data_size() and heap_fill_tuple(), but we need not handle
 * null values here.  TYPE_IS_PACKABLE must test the same conditions as
 * heaptuple.c's ATT_IS_PACKABLE macro.
 */

/* Does datatype allow packing into the 1-byte-header varlena format? */
#define TYPE_IS_PACKABLE(typlen, typstorage) \
	((typlen) == -1 && (typstorage) != 'p')

/*
 * Increment data_length by the space needed by the datum, including any
 * preceding alignment padding.
 */
static Size
datum_compute_size(Size data_length, Datum val, bool typbyval, char typalign,
				   int16 typlen, char typstorage)
{
	if (TYPE_IS_PACKABLE(typlen, typstorage) &&
		VARATT_CAN_MAKE_SHORT(DatumGetPointer(val)))
	{
		/*
		 * we're anticipating converting to a short varlena header, so adjust
		 * length and don't count any alignment
		 */
		data_length += VARATT_CONVERTED_SHORT_SIZE(DatumGetPointer(val));
	}
	else
	{
		data_length = att_align_datum(data_length, typalign, typlen, val);
		data_length = att_addlength_datum(data_length, typlen, val);
	}

	return data_length;
}

/*
 * Write the given datum beginning at ptr (after advancing to correct
 * alignment, if needed).  Return the pointer incremented by space used.
 */
static Pointer
datum_write(Pointer ptr, Datum datum, bool typbyval, char typalign,
			int16 typlen, char typstorage)
{
	Size		data_length;

	if (typbyval)
	{
		/* pass-by-value */
		ptr = (char *) att_align_nominal(ptr, typalign);
		store_att_byval(ptr, datum, typlen);
		data_length = typlen;
	}
	else if (typlen == -1)
	{
		/* varlena */
		Pointer		val = DatumGetPointer(datum);

		if (VARATT_IS_EXTERNAL(val))
		{
			/*
			 * Throw error, because we must never put a toast pointer inside a
			 * range object.  Caller should have detoasted it.
			 */
			elog(ERROR, "cannot store a toast pointer inside a range");
			data_length = 0;	/* keep compiler quiet */
		}
		else if (VARATT_IS_SHORT(val))
		{
			/* no alignment for short varlenas */
			data_length = VARSIZE_SHORT(val);
			memcpy(ptr, val, data_length);
		}
		else if (TYPE_IS_PACKABLE(typlen, typstorage) &&
				 VARATT_CAN_MAKE_SHORT(val))
		{
			/* convert to short varlena -- no alignment */
			data_length = VARATT_CONVERTED_SHORT_SIZE(val);
			SET_VARSIZE_SHORT(ptr, data_length);
			memcpy(ptr + 1, VARDATA(val), data_length - 1);
		}
		else
		{
			/* full 4-byte header varlena */
			ptr = (char *) att_align_nominal(ptr, typalign);
			data_length = VARSIZE(val);
			memcpy(ptr, val, data_length);
		}
	}
	else if (typlen == -2)
	{
		/* cstring ... never needs alignment */
		Assert(typalign == 'c');
		data_length = strlen(DatumGetCString(datum)) + 1;
		memcpy(ptr, DatumGetPointer(datum), data_length);
	}
	else
	{
		/* fixed-length pass-by-reference */
		ptr = (char *) att_align_nominal(ptr, typalign);
		Assert(typlen > 0);
		data_length = typlen;
		memcpy(ptr, DatumGetPointer(datum), data_length);
	}

	ptr += data_length;

	return ptr;
}

/*
 * Transform Enumset type to varlena based EnumsetType structure
 *
 */
static EnumsetType *
enumset_serialize(Enumset *es)
{
	EnumsetType *result;
	Size msize = 0;
	char *ptr;

	if (es->bms == NULL && es->array == NULL)
	{
		/* empty set */
		result = (EnumsetType *) palloc(sizeof(EnumsetType));
		result->typmod = es->typmod;
		SET_VARSIZE(result, sizeof(EnumsetType));
	}
	else if (es->array != NULL)
	{
		/* unspecified not integer set */
		msize = sizeof(EnumsetType);
		msize = datum_compute_size(msize, PointerGetDatum(es->array), false, 'i',  -1, 'x');
		result = (EnumsetType *) palloc0(msize);
		SET_VARSIZE(result, msize);
		result->typmod = TEXTARRAYOID;

		ptr = (char *) (result + 1);
		ptr = datum_write(ptr, PointerGetDatum(es->array), false, 'i', -1, 'x');
	}
	else if (es->bms != NULL)
	{
		/* integer or enum set */
		msize = es->bms->nwords * sizeof(bitmapword);
		result = palloc(msize + sizeof(EnumsetType));
		ptr = (char *) (result + 1);
		SET_VARSIZE(result, msize + sizeof(EnumsetType));
		result->typmod = es->typmod;
		memcpy(ptr, es->bms->words, msize);
	}

	return result;
}

/*
 * Transform Varlena EnumsetType to user friendly Enumset type
 */
static Enumset *
enumset_deserialize(EnumsetType *est)
{
	Size msize = VARSIZE(est) - sizeof(EnumsetType);
	Enumset *result = palloc(sizeof(Enumset));

	if (msize == 0)
	{
		result->typmod = est->typmod;
		result->bms = NULL;
		result->array = NULL;
	}
	else if (est->typmod == TEXTARRAYOID)
	{
		char *ptr;

		result->typmod = est->typmod;

		ptr = palloc(msize);
		memcpy(ptr, (char *) (est + 1), msize);

		result->array = DatumGetArrayTypeP(PointerGetDatum(ptr));
		result->bms = NULL;
	}
	else 
	{
		int nwords;

		result->typmod = est->typmod;
		nwords = msize / sizeof(bitmapword);
		result->bms = (Bitmapset *) palloc(BITMAPSET_SIZE(nwords));
		memcpy(result->bms->words, (char *) (est + 1), msize);
		result->bms->nwords = nwords;
		result->array = NULL;
	}

	return result;
}

/*
 * We reuse array IO functions for Enumset IO
 *
 */
static EnumsetIOData *
get_enumset_io_data(FunctionCallInfo fcinfo, Oid enum_array_typid, IOFuncSelector func)
{
	EnumsetIOData *cache = (EnumsetIOData *) fcinfo->flinfo->fn_extra;

	if (cache == NULL || cache->typmod != enum_array_typid)
	{
		int16	typlen;
		bool	typbyval;
		char	typalign;
		char	typdelim;

		cache = (EnumsetIOData *) MemoryContextAlloc(fcinfo->flinfo->fn_mcxt,
										    sizeof(EnumsetIOData));

		cache->typmod = enum_array_typid;
		get_type_io_data(cache->typmod,
					    func,
					    &typlen,
					    &typbyval,
					    &typalign,
					    &typdelim,
					    &cache->typioparam,
					    &cache->typiofunc);

		if (!OidIsValid(cache->typiofunc))
		{
			/* this could only happen for receive or send */
			if (func == IOFunc_receive)
				ereport(ERROR,
						(errcode(ERRCODE_UNDEFINED_FUNCTION),
						 errmsg("no binary input function available for type %s",
								format_type_be(cache->typmod))));
			else
				ereport(ERROR,
						(errcode(ERRCODE_UNDEFINED_FUNCTION),
						 errmsg("no binary output function available for type %s",
								format_type_be(cache->typmod))));
		}
		fmgr_info_cxt(cache->typiofunc, &cache->proc,
					  fcinfo->flinfo->fn_mcxt);

		fcinfo->flinfo->fn_extra = (void *) cache;
	}

	return cache;
}

/*
 * PostgreSQL usually doesn't send typmod to input functions. Typmod is attached to value
 * in next step - in conversion function. This cast function can expect so input parameter
 * will be a text array or bitmap (when customer data are set of integers). When typmod is
 * valid, then it will transform text' array to bitmap.
 *
 */
Datum
enumset_typmod(PG_FUNCTION_ARGS)
{
	Enumset	   *es = PG_GETARG_ENUMSET_PP(0);
	int32	   typmod = PG_GETARG_INT32(1);
	bool	   is_explicit = PG_GETARG_BOOL(2);

	/* don't allow silent cast between different known enumsets */
	if (es->typmod != -1 && es->typmod != TEXTARRAYOID && !is_explicit && es->typmod != typmod)
		ereport(ERROR,
				(errcode(ERRCODE_DATA_EXCEPTION),
				 errmsg("casting of enumset requires explicit cast")));

	if (es->typmod != typmod)
	{
		if (es->array != NULL)
		{
			Assert(es->typmod == TEXTARRAYOID);

			/* Continue only when target is known enum */
			if (typmod != 1)
			{
				ArrayIterator	iterator;
				Datum	  label;
				bool	  isnull;
				Bitmapset *target_bms = NULL;

				/* verify target type */
				if (!type_is_enum((Oid) typmod))
					ereport(ERROR,
							(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
							 errmsg("type identifier must be enum type")));

				/* array to enum set */
				iterator = array_create_iterator(es->array, 0);
				while (array_iterate(iterator, &label, &isnull))
				{
					char	*label_str;
					HeapTuple	tup;
					Oid	enumoid;

					label_str = TextDatumGetCString(label);

					tup = SearchSysCache2(ENUMTYPOIDNAME,
										ObjectIdGetDatum((Oid) typmod),
										CStringGetDatum(label_str));
					if (!HeapTupleIsValid(tup))
						ereport(ERROR,
							(errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
							 errmsg("invalid input value for enum %s: \"%s\"",
										    format_type_be(typmod),
										    label_str)));

					enumoid = HeapTupleGetOid(tup);
					ReleaseSysCache(tup);
					pfree(label_str);

					/*
					 * Bitmapset is expensive for high integers. But there are possible
					 * effective optimization. We know, so Oid of any enums are little bit
					 * higher than Oid of Enum type. So we can substract typmod (Enum type oid)
					 */
					target_bms = bms_add_member(target_bms, (int) enumoid - typmod);
				}

				array_free_iterator(iterator);

				es->typmod = typmod;
				es->bms = target_bms;
				es->array = NULL;
			}
		}
		else if (es->bms != NULL)
		{
			if (es->typmod != -1 && typmod != -1)
			{
				int	x;
				Bitmapset	*target_bms = NULL;
				Oid		enumoid;

				/* veryfy target type is enum */
				if (!type_is_enum(typmod))
					ereport(ERROR,
							(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
							 errmsg("type identifier must be enum type")));

				/*
				 * IO transformation of enums.
				 */
				while ((x = bms_first_member(es->bms)) >= 0)
				{
					HeapTuple	tup;
					Form_pg_enum	en;
					char		*label;

					tup = SearchSysCache1(ENUMOID, ObjectIdGetDatum((Oid) (x + es->typmod)));
					if (!HeapTupleIsValid(tup))
						ereport(ERROR,
								(errcode(ERRCODE_INVALID_BINARY_REPRESENTATION),
								 errmsg("invalid internal value for enum: %u",
												    x)));
					en = (Form_pg_enum) GETSTRUCT(tup);

					label = NameStr(en->enumlabel);
					ReleaseSysCache(tup);

					tup = SearchSysCache2(ENUMTYPOIDNAME,
										ObjectIdGetDatum((Oid) typmod),
										CStringGetDatum(label));
					if (!HeapTupleIsValid(tup))
						ereport(ERROR,
								(errcode(ERRCODE_INVALID_TEXT_REPRESENTATION),
								 errmsg("invalid input value for enum %s: \"%s\"",
										    format_type_be(typmod),
										    label)));

					enumoid = HeapTupleGetOid(tup);
					ReleaseSysCache(tup);

					target_bms = bms_add_member(target_bms, (int) enumoid - (int) typmod);
				}

				es->typmod = typmod;
				es->bms = target_bms;
			}
			else
			{
				/*
				 * original enum is not defined, but original data are integers, because
				 * are stored in bitmapset - and we cannot to cast between int and enums.
				 */
				ereport(ERROR,
						(errcode(ERRCODE_DATA_EXCEPTION),
						 errmsg("cannot to cast between integer and enum")));
			}
		}
		else
		{
			/* empty input set, we can change typmod simply */
			es->typmod = typmod;
		}
	}

	PG_RETURN_ENUMSET_P(es);
}

/*
 * We expect same input format as text array -  so we can use a array IO functions. Because
 * implementation of enumset is used on agressive using of typmod, and we cannot to get
 * target typmod in input function, we should to store custom data in text' array first.
 * Next we will try to convert this array to integer set or return text array as is.
 *
 */
Datum
enumsetin(PG_FUNCTION_ARGS)
{
	char	*str = PG_GETARG_CSTRING(0);
	Enumset		result;
	ArrayType	*array_enums;
	Datum		array_enums_datum;
	EnumsetIOData		*cache;
	ArrayIterator		iterator;
	Datum		value;
	bool		isnull;
	Bitmapset *bms = NULL;
	bool	is_empty_set = true;

	cache = get_enumset_io_data(fcinfo, TEXTARRAYOID, IOFunc_input);

	/* call a array input function */
	array_enums_datum = InputFunctionCall(&cache->proc, str,
								  cache->typioparam, -1);

	array_enums = DatumGetArrayTypeP(array_enums_datum);

	if (array_contains_nulls(array_enums))
		ereport(ERROR,
				(errcode(ERRCODE_NULL_VALUE_NOT_ALLOWED),
				 errmsg("set of enum must not contail nulls")));

	if (ARR_NDIM(array_enums) > 1)
		ereport(ERROR,
				(errcode(ERRCODE_ARRAY_SUBSCRIPT_ERROR),
				 errmsg("nested sets are not allowed")));

	iterator = array_create_iterator(array_enums, 0);

	/*
	 * We can try to build integer bitmapset
	 */
	while (array_iterate(iterator, &value, &isnull))
	{
		char *label;
		long		l;
		char	   *badp;

		is_empty_set = false;

		label = TextDatumGetCString(value);

		errno = 0;
		l = strtol(label, &badp, 10);

		if (label == badp)
		{
			bms = NULL;
			break;
		}

		while (*badp && *badp != 0 && isspace((unsigned char) *badp))
			badp++;

		if (*badp && *badp != 0)
		{
			bms = NULL;
			break;
		}


		if (errno == ERANGE
#if defined(HAVE_LONG_INT_64)
			/* won't get ERANGE on these with 64-bit longs... */
			|| l < INT_MIN || l > INT_MAX
#endif
			)
			ereport(ERROR,
					(errcode(ERRCODE_NUMERIC_VALUE_OUT_OF_RANGE),
				errmsg("value \"%s\" is out of range for type integer", label)));

		bms = bms_add_member(bms, (int32) l);
	}

	array_free_iterator(iterator);

	if (is_empty_set)
	{
		result.typmod = -1;
		result.bms = NULL;
		result.array = NULL;
	}
	else
	{
		if (bms != NULL)
		{
			result.typmod = -1;
			result.bms = bms;
			result.array = NULL;
		}
		else
		{
			result.typmod = (int32) TEXTARRAYOID;
			result.bms = NULL;
			result.array = array_enums;
		}
	}

	PG_RETURN_ENUMSET_P(&result);
}


Datum
enumsetout(PG_FUNCTION_ARGS)
{
	ArrayBuildState *astate = NULL;
	Enumset		*es = PG_GETARG_ENUMSET_PP(0);
	Datum	array_enums_datum;
	int	x;
	char	*output_str;
	EnumsetIOData *cache;

	if (es->typmod == -1 && es->bms != NULL)
	{
		cache = get_enumset_io_data(fcinfo, INT4ARRAYOID, IOFunc_output);
		/* convert to int4 array */
		while ((x = bms_first_member(es->bms)) >= 0)
		{
			astate = accumArrayResult(astate,
						Int32GetDatum(x), false,
						INT4OID,
						CurrentMemoryContext);
		}

		array_enums_datum = makeArrayResult(astate, CurrentMemoryContext);
		output_str = OutputFunctionCall(&cache->proc, array_enums_datum);
	}
	else if (es->typmod != -1 && es->bms != NULL)
	{
		cache = get_enumset_io_data(fcinfo, TEXTARRAYOID, IOFunc_output);
		/* convert enum set to array */
		while ((x = bms_first_member(es->bms)) >= 0)
		{
			HeapTuple	tup;
			Form_pg_enum	en;
			char		*label;

			tup = SearchSysCache1(ENUMOID, ObjectIdGetDatum((Oid) x + es->typmod));
			if (!HeapTupleIsValid(tup))
				ereport(ERROR,
						(errcode(ERRCODE_INVALID_BINARY_REPRESENTATION),
						 errmsg("invalid internal value for enum: %u",
											    x)));
			en = (Form_pg_enum) GETSTRUCT(tup);

			label = NameStr(en->enumlabel);
			ReleaseSysCache(tup);

			astate = accumArrayResult(astate,
						CStringGetTextDatum(label), false,
						TEXTOID,
						CurrentMemoryContext);
		}

		array_enums_datum = makeArrayResult(astate, CurrentMemoryContext);
		output_str = OutputFunctionCall(&cache->proc, array_enums_datum);
	}
	else
	{
		if (es->array == 0)
			output_str = pstrdup("{}");
		else
		{
			cache = get_enumset_io_data(fcinfo, TEXTARRAYOID, IOFunc_output);
			output_str = OutputFunctionCall(&cache->proc, PointerGetDatum(es->array));
		}
	}

	PG_RETURN_CSTRING(output_str);
}

Datum
enumset_typmod_in(PG_FUNCTION_ARGS)
{
	ArrayType  *ta = PG_GETARG_ARRAYTYPE_P(0);
	Datum	   *elem_values;
	int			n;
	char		*enum_name;
	Oid		typid;
	int32		typmod;
	int32		result;

	if (ARR_ELEMTYPE(ta) != CSTRINGOID)
		ereport(ERROR,
				(errcode(ERRCODE_ARRAY_ELEMENT_ERROR),
				 errmsg("typmod array must be type cstring[]")));

	if (ARR_NDIM(ta) != 1)
		ereport(ERROR,
				(errcode(ERRCODE_ARRAY_SUBSCRIPT_ERROR),
				 errmsg("typmod array must be one-dimensional")));

	if (array_contains_nulls(ta))
		ereport(ERROR,
				(errcode(ERRCODE_NULL_VALUE_NOT_ALLOWED),
				 errmsg("typmod array must not contain nulls")));

	/* hardwired knowledge about cstring's representation details here */
	deconstruct_array(ta, CSTRINGOID,
					  -2, false, 'c',
					  &elem_values, NULL, &n);

	if (n != 1)
		ereport(ERROR,
				(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
				 errmsg("invalid enumset type modifier")));

	enum_name = DatumGetCString(elem_values[0]);

	/* search a enum datatype */
	parseTypeString(enum_name, &typid, &typmod);

	pfree(elem_values);

	if (!type_is_enum(typid))
		ereport(ERROR,
			(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
			 errmsg("type identifier must be enum type")));

	result = (int32) typid;

	PG_RETURN_INT32(result);
}

Datum
enumset_typmod_out(PG_FUNCTION_ARGS)
{
	int32	typmod = PG_GETARG_INT32(0);
	char *result;

	result = format_type_be((Oid) typmod);

	PG_RETURN_CSTRING(result);
}
