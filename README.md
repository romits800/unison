# Getting Started with DivCon

DivCon is based on [Unison](https://unison-code.github.io/), a software tool that
performs register allocation and instruction scheduling in integration using
combinatorial optimization.

## Prerequisites

Unison has the following dependencies:
[Stack](http://www.haskellstack.org/),
[Qt](https://www.qt.io/) (version 5.x, optional see [#33](https://github.com/unison-code/unison/issues/33)),
[Graphviz library](http://www.graphviz.org/) (also optional), and
[Gecode](http://www.gecode.org/) (version 6.0.0).
To get the first three dependencies in Debian-based distributions, just run:

```
apt-get install haskell-stack qtbase5-dev libgraphviz-dev
```

Upgrade Slack after installing it:

```
stack upgrade
```

The source of Gecode can be fetched with:

```bash
wget https://github.com/Gecode/gecode/archive/release-6.2.0.tar.gz

```

and installed from the sources (a simple procedure is as follows):

```bash
tar xzfv release-6.2.0.tar.gz
cd gecode-release-6.2.0
./configure
make
sudo make install
```
## Cloning
Clone the code from Github:

```bash
git clone https://github.com/romits800/divCon.git
```

## Building

If it is the first time you run `cabal` run:

```bash
cabal update
```

Just go to the `src` directory and run:

```
make build
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

If you have a .mir file you can run DivCon as follows:
```bash
uni import --target=mips factorial.mir -o factorial.uni --function=factorial --maxblocksize=50 --goal=speed
uni linearize --target=mips factorial.uni -o factorial.lssa.uni
uni extend --target=mips  factorial.lssa.uni -o factorial.ext.uni
uni augment --target=mips factorial.ext.uni -o factorial.alt.uni
uni model --target=mips factorial.alt.uni -o factorial.json 
gecode-presolver -o factorial.ext.json -dzn factorial.dzn --verbose factorial.json
```
Run the gecode solver:
```bash
gecode-solver  -o factorial.out.json --verbose factorial.ext.json
```
or the portfolio solver (requires installing [MiniZincIDE-2.2.3-bundle-linux](https://github.com/MiniZinc/MiniZincIDE/releases/tag/2.2.3)):

```bash
export DIVCON_PATH=/path/to/divCon
export MINIZINC_PATH=/path/to/minizincIDE/bin
export PATH=${PATH}:${DIVCON_PATH}/src/solvers/gecode:${DIVCON_PATH}/src/solvers/multi_backend/minizinc/:${DIVCON_PATH}/src/solvers/multi_backend/:${MINIZINC_PATH}:${DIVCON_PATH}/src/solvers/multi_backend/common/ UNISON_DIR=${DIVCON_PATH}
${DIVCON_PATH}/src/solvers/multi_backend/portfolio-solver -o factorial.out.json --verbose factorial.ext.json
```

And run the diversifier for:
* Gap to optimal = 10%
* Diversification algorithm = LNS
..* Relax rate = 70%
..* Restart sequence = constant
..* Restart scale = 500
* Random seed = 123
* Distance = Hamming
* Number of variants = 100

```bash
flags="--disable-copy-dominance-constraints --disable-infinite-register-dominance-constraints --disable-operand-symmetry-breaking-constraints --disable-register-symmetry-breaking-constraints --disable-temporary-symmetry-breaking-constraints --disable-wcet-constraints"

gecode-diversify  ${flags} --acceptable-gap 10 --relax 0.7 --seed 123 --distance hamming --div-method monolithic_lns --restart constant --restart-scale 500 --number-divs 100 --solver-file factorial.out.json --use-optimal-for-diversification  --divs-dir $DIVS_DIR -o factorial.out.json --enable-solver-solution-brancher --branching clrandom  factorial.ext.json
```
This will generate 99 files with names `{0..99}.factorial.out.json`.

## Documentation

### Unison

Check out Unison's work-in-progress
[manual](https://unison-code.github.io/doc/manual.pdf). The manual's source can
be found in the `doc` directory.

Source-level documentation is also available for the core Haskell modules of
Unison (`MachineIR.Base`, `Unison.Base`, and `Unison.Target.API`). To generate
this documentation in HTML format and open it with a web browser, just run:

```
make doc
```

## Contact

<<<<<<< HEAD
[Rodothea Myrsini Tsoupidi](https://www.kth.se/profile/tsoupidi/) [<tsoupidi@kth.se>]
=======
[Roberto Castañeda Lozano](https://robcasloz.github.io/) [<rcas@acm.org>]
>>>>>>> upstream/master

## License

DivCon is licensed under the BSD3 license, see the [LICENSE.md](LICENSE.md) file
for details.


