# Connection to a Sirius instance

Creates a `Sirius` instance and checks that connection to the server is
valid. returns the Api and SDK within its slots. Main object that the
user will interact with to connect to the Sirius server and perform
operations.

Creates a `Sirius` object and checks that the connection to the Sirius
server is valid. If the Sirius server is not running, the function will
attempt to start it using the provided path to the executable. If the
connection is not valid, the function will attempt to log in using the
provided credentials. If the connection is still not valid, the function
will stop with an error message.

## Usage

``` r
Sirius(
  username = character(),
  password = character(),
  projectId = character(),
  path = character(),
  verbose = FALSE
)

# S4 method for class 'Sirius'
show(object)
```

## Arguments

- username:

  `character(1)`, the username to use for the connection

- password:

  `character(1)`, the password to use for the connection

- projectId:

  `character(1)`, the project id to use for the connection

- path:

  `character(1)` path where to find the existing project or where to
  create a new one.By default, the porject will be opened in the current
  `"."` directory.

- verbose:

  `logical(1)`, if `TRUE` the function will print all messages to the
  console. Use if need debug, default is `FALSE`.

- object:

  `Sirius`, the object to show,

## Value

`Sirius` object with the Sirius api connected.

## Slots

- `api`:

  `ANY`, the api object to use for the connection

- `sdk`:

  `ANY`, the sdk object to use for the connection

- `projectId`:

  `character`, the project id to use for the connection

- `featureMap`:

  `data.frame`, the feature map to use for the connection

## Author

Philippine Louail (+people that worked on the API)
