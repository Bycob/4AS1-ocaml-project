# OCaml projet - 4A fall semester

This project aims to develop an implementation of the Ford Fulkerson algorithm and apply it to different use cases.

## Compilation

```bash
ocamlbuild ftest.native
```

## Execution

```bash
./ftest.native [infile] [source] [sink] [outfile]
```

This command runs the test program with given parameters. The result is written to `outfile` in graphviz format.
Use `dot` or `convpng.sh` script to create images out of the dot files.
Some computation steps may be written to the `debug` directory.
