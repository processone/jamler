# Jamler

Jamler is an experimental XMPP server.  It was developed mainly in 2011 as an
attempt to rewrite [ejabberd](https://github.com/processone/ejabberd) in OCaml
to see how static typing would affect it.
As ejabberd has evolved a lot since 2011, please use ejabberd version 2.1.8 to
compare it to jamler.
Currently (2023) it's updated to use actual versions of OCaml and libraries,
but no new features are added.

## Comparision with Erlang and ejabberd

This section compares implementation details, not features.

### XML elements

XML elements were defined in ejabberd like this:
```erlang
-type(xmlelement() :: {xmlelement, name(), attrs(), [xmlelement() | cdata()]}).
-type(cdata() :: {xmlcdata, binary()}).
```

It is similar in jamler:
```ocaml
type element_cdata =
  [ `XmlElement of name * attributes * element_cdata list
  | `XmlCdata of string ]

type element =
  [ `XmlElement of name * attributes * element_cdata list ]
```

### Stringprep

To compare JIDs, they must be normalized first
([RFC6122](https://tools.ietf.org/html/rfc6122)).  There are 3 stringprep
profiles for corresponing JID parts (e.g. `(user, server, resource)` must be
normalized into `(nodeprep(user), nameprep(server), resourceprep(resource))`).
It's error-prone (e.g. using `nodeprep` where `nameprep` is needed, or
foregeting to normalize).  As a result, some ejabberd functions always do
normalization to be on a safe side, and some values are normalized several
times during processing.

Jamler has 3 private types for normalized values:
```ocaml
type namepreped = private string
type nodepreped = private string
type resourcepreped = private string

val nameprep : string -> namepreped option
val nodeprep : string -> nodepreped option
val resourceprep : string -> resourcepreped option
```
So the only way to create e.g. `namepreped` value is via a call to the `nameprep`
function.  Other functions can define that they expect only a normalized value:
```ocaml
val jid_replace_resource' : jid -> resourcepreped -> jid
```
At the same time those values are still strings and can be used as strings:
`(host :> string)`.

### SQL queries

In 2011 ejabberd used manual escaping for SQL queries:
```erlang
Username = ejabberd_odbc:escape(LUser),
ejabberd_odbc:sql_query(
  LServer,
  ["select password from users where username='", Username, "';"]).
```

For jamler camlp4 (and now ppx) syntax extension was implemented to prepare SQL
queries:
```ocaml
let suser = (user : Jlib.nodepreped :> string) in
let query = [%sql {|SELECT @(password)s from users where username = %(suser)s|}] in
...
```
It was so convenient, so later got ported to ejabberd and extended further:
```erlang
ejabberd_sql:sql_query(
  LServer,
  ?SQL("select @(password)s from users where username=%(LUser)s")).
```

Nowadays you can find similar functionality in
[ocaml-sqlexpr](https://github.com/mfp/ocaml-sqlexpr).


### Processes

Erlang processes are mimiced in jamler using Lwt threads.  Processes have the
following type:
```ocaml
type -'a pid
```
where `'a` is a type of messages the process can receive.

Internally `pid` is `int` pointing to a record in the processes table.  That
was probably not a good decision, as forgeting to cleanup pid references can
lead to a bug where another process gets the same pid as a now-dead process and
receives incompatible messages.

API is very similar to erlang:
| Erlang                   | Jamler                                   |
|--------------------------|------------------------------------------|
| `spawn(f)`               | `spawn f`                                |
| `Pid ! Msg`              | `pid $! msg`                             |
| `receive Msg -> ... end` | `match%lwt receive self with msg -> ...` |


### gen_server

Erlang's `gen_server` module is also mimiced.  A callback module should have the
following type:
```ocaml
module type Type =
sig
  type msg
  type state
  type init_data
  type stop_reason
  val init : init_data -> msg pid -> (state, stop_reason) init_result
  val handle : msg -> state -> (state, stop_reason) result
  val terminate : state -> stop_reason -> unit Lwt.t
end
```
Then `gen_server` instance can be created:
```ocaml
module FooServer = Gen_server.Make(Callbacks)
...
FooServer.start init_data
```

### Erlang terms

There are erlang term external format encoding/decoding functions in the
`Erlang` module.  These functions get type description of the encoded/decoded
value:
```ocaml
ErlType.(from_term (list atom) nodes)
```
They are based on this article:
http://okmij.org/ftp/ML/first-class-modules/#generics


### Erlang distribution

Jamler was able to connect to epmd and other erlang nodes and exchange
messages.  Currently connecting from another erlang node fails with
```
Connection attempt to node jamler@localhost aborted since it cannot handle ["UTF8_ATOMS", "NEW_FUN_TAGS"].
```

## Compiling and running

Use opam to install dependencies:

```
opam install yojson lwt lwt_log lwt_ppx lwt_ssl cryptokit pgocaml ppxlib
```

Also install development libraries for expat and GNU Libidn.  E.g. in Debian:
```
apt-get install libidn11-dev libexpat1=dev
```

Then run `make`.  Create a config basing on `jamler.cfg.example` and start it with
```
_build/default/src/main.exe -c jamler.cfg
```

It requires PostgreSQL to run, use
[schema](https://github.com/processone/ejabberd/blob/v2.1.8/src/odbc/pg.sql)
from ejabberd.
