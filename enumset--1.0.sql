-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION enumset" to load this file. \quit

--
--  PostgreSQL code for CITEXT.
--
-- Most I/O functions, and a few others, piggyback on the "text" type
-- functions via the implicit cast to text.
--

--
-- Shell type to keep things a bit quieter.
--

CREATE TYPE enumset;

--
--  Input and output functions.
--
CREATE FUNCTION enumsetin(cstring)
RETURNS enumset
AS 'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION enumsetout(enumset)
RETURNS cstring
AS 'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION enumset_typmod_in(cstring[])
RETURNS integer
AS 'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION enumset_typmod_out(integer)
RETURNS cstring
AS 'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION enumset_typmod(enumset, int, bool)
RETURNS enumset
AS 'MODULE_PATHNAME'
LANGUAGE C IMMUTABLE STRICT;

--
--  The type itself.
--

CREATE TYPE enumset (
    INPUT          = enumsetin,
    OUTPUT         = enumsetout,
    TYPMOD_IN      = enumset_typmod_in,
    TYPMOD_OUT     = enumset_typmod_out,
    INTERNALLENGTH = VARIABLE,
    STORAGE        = extended,
    -- make it a non-preferred member of string type category
    CATEGORY       = 'A',
    PREFERRED      = false,
    COLLATABLE     = false
);

CREATE CAST (enumset AS enumset)      
WITH FUNCTION enumset_typmod(enumset, int, bool) AS IMPLICIT;
