# FPTalks

`FPTalks` is my web application (messenger) for [Tensor](https://tensor.ru)
written in **Haskell** using **Yesod** and **SQLite**.

## Project setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
In Unix-like operating systems, you can also install the `stack`
(`haskell-stack`, `stack-static`, etc.) from the repository (You're not going to
run a web application server under Windows, are you?)
2. Install GHC (compiler and interpreter): `stack setup`
3. Build project with dependencies: `stack build`
4. Run `fptalks` using `stack exec fptalks`

## Description

Since Haskell isn't very popular in web development, I'll write a few words here
about the language itself and the tools used.

### Haskell

`Haskell` is a very high-level, multi-paradigm language with implicit static
polymorphic typing. Haskell has a very original syntax:
```
-- Function application, equal to f(x, y, x * y) in C style
f x y (x * y)

-- Equal expressions
arg0 `function` arg1
function arg0 arg1

-- Equal definitions
f unused0 unused1 arg2 = g arg2
f _ _ arg2 = g arg2
f _ _ = \ arg2 -> g arg2
f _ _ = g
```

* `Combinators` (aka. higher-order functions) are one of the ways in Haskell to
take away some parentheses, indentation, branching constructs, etc. For example,
`(.)` and `($)` allow you to chain functions:
```
-- Equal definitions
provideRep . return $ object ["message" .= notFound']
provideRep (return (object ["message" .= notFound']))
```
`(.)` and `($)` works with any data type, other combinators may be more complex:
```
-- Dummy Haskell
isUser :: Handler AuthResult
isUser =  do
  mid <- maybeAuthId
  return $ if mid == Nothing then AuthenticationRequired else Authorized

{- |
  Clever Haskell, with base library definitions:
  
  function <$> action = do result <- action; return (function result)
  
  maybe default f    Nothing   = default -- if null
  maybe default f (Just value) = f value -- if not null
  
  const x y = x
-}
isUser :: Handler AuthResult
isUser =  AuthenticationRequired `maybe` const Authorized <$> maybeAuthId

// Java-style
public boolean isUser ()
{
  return this.maybeAuthId() == null ? AuthResult.AuthenticationRequired : AuthResult.Authorized
}
```
* `Monads` are a form of mathematical magic that allows you to write procedures,
handle exceptions, make secure transactions, describe the content of a web page,
write logs, keep some data inside and much more. Monad are something like a
simple class that hide part of the logic and additional data, work with internal
state and perform only one simple task. In fact, the monad defines three things:
1. How to bind some stateless data with an action
2. How to combine two such monadic values (actions) into one (how to combine
messages in a log, DB transactions, etc.)
3. Some public API (monad-dependent). For example, the `Maybe` monad contains
the constant `Nothing`, equivalent of `null` (types in Haskell have no
predefined `null`).
In this case, it just a concatenation of some styles. Of course, monads can nest
in one another and form more powerful monads - functions like `return` for such
monads can "jump" several levels and even to a different number of levels,
depending on the context.
* `do`-notation is syntactic sugar, which allows to write monadic structure in
an imperative (procedural) style. `Monad` definition overloads the `return`
function and `do`-notation behavior, generalizing the concept of a procedure to
a huge class of very diverse operations.
```
{-
  Get SQL connection from running application and after run action.
  Here the do block is executed in the Handler monad, which describes the server
  request handler.
-}
runDB action = do
  serverData <- getYesod
  runSqlConn action (sqlBackend serverData)

-- Short version
runDB action = getYesod >>= runSqlConn action . sqlBackend

// Java-style
public void runDB(Function<SqlBackend, Void> action)
{
  getYesod.sqlBackend.runSqlConn(action);
}

{-
  Here the do block is executed in the Widget monad, which describes page
  content templates. In this case, monad used as concat.
-}
fptalksStyleW = do clearStyleW; pageStyleW; formStyleW; boxStyleW
```
* `Template Haskell` (TH) is a metaprogramming extension that evaluates and
embeds arbitrary definitions at compile time
* `QuasiQuoters` (QQ) is another metaprogramming extension that adds the block
notation for TH. QQ embeds blocks of arbitrary DSL (domain-specific language)
code into the language.
* `Type classes` are the most familiar thing in Haskell for most developers.
Type classes are prototype for interfaces in `Java`, `C#` and other languages.
Type classes have an inheritance system, rules for function behavior and
relationships, many functions in classes also have default definitions.
* `Type families` are similar to type classes, but they define operations on
types, not values. Type families can describe one-to-one and one-to-many
relationships. For example, `YesodPersistBackend` describes a relationship of
types that describes a web application and a SQL database, and `Route` describes
a relationship between a web application and server requests (GET, POST, etc.).

* `Yesod` is a web framework written in pure Haskell using very powerful
language features such as Template Haskell with QuasiQuoters, type families and
type classes

## Roadmap

* The main module - `FPTalks` - connects to the database and initializes the web
server. For this project, I'm using SQLite and the `warp` web server written in
Haskell. It's well suited for training projects and requires no configuration.
For more complex projects, you can use, for example, `nginx` or `apache`
(although I haven't tried to configure and run project with them).
* `Foundation` module provides request and database stuff for foundation type
`FPTalks` (represents web app).
* `Decoration` Decoration provides well-formatted versions of standard pages, so
the reviewer doesn't need to delve into the logic of the actions being
performed - it doesn't differ from the logic in the usual behavior of an `Yesod`
application.
* `Sendmail` provides mail sender (calls the `sendmail` utility with standard
parameters). If the verifier doesn't have `sendmail` or has problems with its
configuration/use, the code starting from the line
```
liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
```
can be commented out. For ease of debugging, the application also displays the
verification link in the console, the line is responsible for this:
```
liftIO $ putStrLn $ "Copy / Paste this URL in your browser:" ++ unpack verurl
```
* The styles are in the `Styles` module, they are written using `cassius` - a
CSS generating QQ template. The `cassius` syntax is just an indentation
sensitive CSS variant.

Additional files:
* `client_session_key.aes` is `Yesod` file
* `stack.yaml` and `stack.yaml.lock` is package manager files
* `fptalks.cabal` is declarative project file (YAML-like format), describing
library and application components of `FPTalks`

## Documentation

* You can read about [Haskell](https://haskell.org) and
[Yesod](https://www.yesodweb.com/book) online
* You can read `Yesod` documentation [here](http://stackage.org/) or
[here](https://hackage.haskell.org/package/yesod-core)
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your
	dependencies and open that documentation in a browser
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample
code for various needs

