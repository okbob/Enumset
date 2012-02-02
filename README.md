# Enumset

This is a PostgreSQL's extension that enable a set of enums

## Usage

    postgres=# create type colors as enum('red','white','yellow','blue');
    CREATE TYPE
    postgres=# select '{red, white}'::enumset(colors);
      enumset   
    -------------
     {red,white}
    (1 row)

    postgres=# select '{red, white, nocolor}'::enumset(colors);
    ERROR:  invalid input value for enum colors: "nocolor"

    postgres=# select '{}'::enumset(colors);
     enumset 
     ---------
      {}
    (1 row)
