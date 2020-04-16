# Getting Started with DivCon

DivCon is based on [Unison](https://unison-code.github.io/), a software tool that
performs register allocation and instruction scheduling in integration using
combinatorial optimization.

## Prerequisites

Unison has the following dependencies:
[Haskell platform](http://hackage.haskell.org/platform/),
[Qt](https://www.qt.io/) (version 4.x, optional see [#33](https://github.com/unison-code/unison/issues/33)),
[Graphviz library](http://www.graphviz.org/), and
[Gecode](http://www.gecode.org/) (version 6.2.0).
To get the first three dependencies in Debian-based distributions, just run:

```
apt-get install haskell-platform libqt4-dev libgraphviz-dev
```

The source of Gecode can be fetched with:

```
wget https://github.com/Gecode/gecode/archive/release-6.2.0.tar.gz
```

## Building

Just go to the `src` directory and run:

```
make build
```

## Testing

Unison contains a test suite with a few functions where different targets and
optimization goals are exercised. To execute the tests just run:

```
make test
```

## Installing

The building process generates three binaries. The installation process consists
in copying the binaries into the appropriate system directory. To install the
binaries under the default directory `usr/local` just run:

```
make install
```

The installation directory is specified by the Makefile variable `PREFIX`. To
install the binaries under an alternative directory `$DIR` just run:

```
make install PREFIX=$DIR
```

## Running

## Documentation

### Unison

Check out Unison's work-in-progress
[manual](https://unison-code.github.io/doc/manual.pdf). The manual's source can
be found in the `doc` directory.

Source-level documentation is also available for the core Haskell modules of
Unison (`MachineIR.Base`, `Unison.Base`, and `Unison.Target.API`). To generate
this documentation in HTML format, just run:

```
make doc
```

## Contact

[Rodothea Myrsini Tsoupidi](https://www.kth.se/profile/tsoupidi/) [<tsoupidi@kth.se>]

## License

Unison is licensed under the BSD3 license, see the [LICENSE.md](LICENSE.md) file
for details.


