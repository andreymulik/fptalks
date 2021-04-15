# FPTalks

`FPTalks` is my web application (messenger) for [Tensor](https://tensor.ru)
written in **Haskell** using **Yesod** web frameword and **Persistent** sqlite
toolkit.

## Project setup

1. If you haven't already, install [Haskell Tool Stack](https://haskell-lang.org/get-started)
In Unix-like operating systems, you can also install the `stack`
(`haskell-stack`, `stack-static`, etc.) package from the repository (I hope you
don't need to run web server on Windows machine)
2. Install GHC (compiler and interpreter): `stack setup`
3. Build project with dependencies: `stack build`
4. Run `fptalks` using `stack exec fptalks` command

## Project

* The main module - `FPTalks` - connects to the database, load static files and
initializes web server. For this project, I'm using `SQLite` and `warp` (web
server written in Haskell). It's well suited for training projects and requires
no configuration. For more complex projects, you can use, for example, `nginx`
or `apache` (although I haven't tried to configure and run project with them).
* `Foundation` module provides request handlers, form definitions and database
stuff for foundation type `FPTalks` (represents web app state).
* `Decoration` provides well-formatted versions of standard pages, so the
reviewer doesn't need to delve into the logic of the actions being performed -
it doesn't differ from the logic in the usual behavior of an `Yesod` application.
* `Sendmail` provides mail sender (calls the `sendmail` util with default
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
* The styles are in the `Styles` module (obviosly), they are written using
`cassius` - a CSS generating QuasiQuoter template. The `cassius` syntax is just
an indentation sensitive CSS variant.

Additional files:
* `client_session_key.aes` is `Yesod` session keys
* `stack.yaml` and `stack.yaml.lock` is `Haskell` solution files
* `fptalks.cabal` is declarative project file (`YAML`-like format), describing
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

