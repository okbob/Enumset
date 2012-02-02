
-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION enumset" to load this file. \quit

ALTER EXTENSION enumset ADD type enumset;
ALTER EXTENSION enumset ADD function enumsetin(cstring);
ALTER EXTENSION enumset ADD function enumsetout(citext);
ALTER EXTENSION enumset ADD function enumset_typmod_in(cstring[]);
ALTER EXTENSION enumset ADD function enumset_typmod_out(int);
ALTER EXTENSION enumset ADD function enumset_typmod(enumset, int, bool);
