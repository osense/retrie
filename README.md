## Retrie
Radix trees with UTF-8 and arbitrary regex support implemented in Erlang.

#### Pros
- fail fast
- ability to define retries from YAML

#### Cons
- large space complexity (mainly due to the use of arrays)
- each regex in a pattern poses a linear slowdown

## Usage

Patterns are constructed from character literals and regular expressions. The regexes are inserted using special syntax:
```
%{[A-Z0-9]+:[a-zA-Z0-9_]+}
```
where the part between `%{` and `:` is the regex type and the part between `:` and `}` is the regex name, e.g. `Hello, %{STRING:name}!` is a valid pattern
and matches, for example, the string `Hello, World!`.

### Using predefined regex types
These are only meant to be used for testing purposes. The predefined regular expressions are:

- STRING
  - `[\\p{L}0-9]+`
  - matches are returned as UTF-8 encoded binaries
- INT
  - `[-+]?[[:digit:]]+`
  - matches are returned as integers
- FLOAT
  - `[-+]?([[:digit:]]\.)?[[digit]]+`
  - matches are returned as floats
- BOOL
  - `true|false`
  - matches are returned as the atoms `true` and `false`

#### Basic example

```
T1 = retrie:insert_pattern(<<"%{STRING:name}, helló!"/utf8>>, p1, retrie:new()),
{p1, [{<<"name">>, <<"World">>}]} = retrie:lookup_match(<<"World, helló!"/utf8>>, T1),
```
```
T2 = retrie:insert_pattern(<<"Hello %{STRING:name} id: %{INT:id}">>, p2, T1),
{p2, [{<<"name">>, <<"Foo">>}, {<<"id">>, 34}]} = retrie:lookup_match(<<"Hello Foo id: 34">>, T2)).
```

### Defining retries in YAML
When dealing with larger amounts of patterns, it is more efficient to define these in a YAML file, which is then loaded by the `retrie_patterns` module.
This YAML file also has to include regex definitions (it is also the only way to provide custom regex definitions, barring direct source code modification).
The structure of the YAML file is:
```
regexes:
  <NAME1> : [<type1>, <regex_string1>]
  <NAME2> : [<type2>, <regex_string2>]
  ...

patterns:
  <retrie_name1>:
    <pattern_name1> : <string1>
    <pattern_name2> : <string2>
  <retrie_name2>:
    ...
  ...
```
where `<typeN>` can be one of:
- string
- integer
- float
- boolean

This type information is used to convert the matching string to the requested data type (`string` essentially performs no conversion whatsoever).

#### YAML example
Given `test.yaml`, which defines a single retrie called `test`:
```
regexes:
  STRING   : [string, '\p{L}+']
  INT      : [integer, '[[:digit:]]+']
  BOOL     : [boolean, 'true|false']

patterns:
  test:
    hello   : 'Hello, %{STRING:name}'
    from    : '%{STRING:msg} from %{STRING:from}:%{INT:port} is %{BOOL:valid}.'
```
it is then loaded and used thusly:
```
T = retrie_patterns:load_group("test.yaml", <<"test">>),
{<<"hello">>, [{<<"name">>, <<"Abca">>}]} = retrie:lookup_match(<<"Hello, Abca">>, T)).
```