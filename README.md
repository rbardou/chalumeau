# Chalumeau

An experimental library to build OCaml libraries and programs.

Also provides an executable, `chalumeau`, which finds a file named
`make.ml` in the current directory or one of its ancestors, and
which runs `ocaml` on it with the right options. It allows you to build
your project easily by running `chalumeau`, but it is definitely not
mandatory.

See the blog post: http://romain.bardou.fr/blog/Designing_Chalumeau.html

## Compile

Just run:

    make

Or, if you don't have make:

    ocaml make.ml

## Examples

The best example is `make.ml` which builds Chalumeau itself.

## Status

I plan to:
- add more examples;
- clean up the library, including function names and its documentation;
- add support for parallel builds;
- automatically find and link modules which can be found using findlib;
- ...

But as I said, it is currently more of an experimentation than anything else.
It is definitely not stable. It is not even released nor available in Opam.

## License

Chalumeau is released under the MIT license.
See the `LICENSE` file.
