# Getting Started with SecConCG

SecConCG is a tool to generate code that is secure against power side-channel (PSC) attacks. 
In particular it generates code that is free from transitional effects among registers 
and the memory bus.

SecConCG is based on [Unison](https://unison-code.github.io/), a combinatorial compiler backend that
performs register allocation and instruction scheduling in integration.

## Prerequisites

SecConCG has the following dependencies:
[Stack](http://www.haskellstack.org/),
[Qt](https://www.qt.io/) (version 5.x, optional see [#33](https://github.com/unison-code/unison/issues/33)),
[Graphviz library](http://www.graphviz.org/) (also optional),
[Gecode](http://www.gecode.org/) (version 6.2.0),
[Arpack](https://rcc.fsu.edu/software/arpack) (version 3.0.7-2)
[Openblas](https://www.openblas.net/) (version 0.3.5)

To get the first three dependencies in Debian-based distributions, just run:

```
apt-get install haskell-stack qtbase5-dev libgraphviz-dev libarpack2-dev libopenblas-dev build-essential qt5-default
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

| you may need to add the installed path to the `LD_LIBRARY_PATH` environment variable, e.g. `export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib` |
| --- |



Download Minizinc for the portfolio solver ([MiniZincIDE-2.2.3-bundle-linux](https://github.com/MiniZinc/MiniZincIDE/releases/tag/2.2.3)),
extract the precompiled version for linux 
and set the environment variable `$MINIZINC_PATH`:

```bash
export MINIZINC_PATH=/path/to/minzincIDE/bin
```


## Cloning
Clone the code from Github:

```bash
git clone -b secconcg https://github.com/romits800/divCon.git
```

## Building


Navigate to the `src` directory and run:

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

If you have a .mir file you can run SecConCG as follows:
```bash
uni import --target=Mips masked_xor.mir -o masked_xor.uni --function=xor --maxblocksize=20 --goal=speed --policy input.txt
uni linearize --target=Mips masked_xor.uni -o masked_xor.lssa.uni
uni extend --target=Mips  masked_xor.lssa.uni -o masked_xor.ext.uni
uni augment --target=Mips masked_xor.ext.uni -o masked_xor.alt.uni
uni model --target=Mips masked_xor.alt.uni -o masked_xor.json --policy input.txt
gecode-presolver -o masked_xor.ext.json -dzn masked_xor.dzn --verbose masked_xor.json
```
Run the gecode PSC-aware solver:
```bash
# disable symmetry breaking constraints
flags=--disable-copy-dominance-constraints --disable-infinite-register-dominance-constraints --disable-operand-symmetry-breaking-constraints --disable-register-symmetry-breaking-constraints --disable-temporary-symmetry-breaking-constraints --disable-wcet-constraints
# select implementation
flags="$flags --sec-implementation sec_reg_2_mem_2" 
gecode-secsolver $flags -o masked_xor.out.json --verbose masked_xor.ext.json
```
or the portfolio solver (requires installing [MiniZincIDE-2.2.3-bundle-linux](https://github.com/MiniZinc/MiniZincIDE/releases/tag/2.2.3)):

```bash
export SECCON_PATH=/path/to/seccon
export MINIZINC_PATH=/path/to/minizincIDE/bin
export PATH=${PATH}:${SECCON_PATH}/src/solvers/gecode:${SECCON_PATH}/src/solvers/multi_backend/minizinc/:${SECCON_PATH}/src/solvers/multi_backend/:${MINIZINC_PATH}:${SECCON_PATH}/src/solvers/multi_backend/common/ UNISON_DIR=${SECCON_PATH}
${SECCON_PATH}/src/solvers/multi_backend/portfolio-solver -o masked_xor.out.json --verbose masked_xor.ext.json
```
Generate the assembly code:
```bash
uni export --target=Mips --keepops masked_xor.alt.uni -o masked_xor.unison.mir --solfile=masked_xor.out.json
flags="-disable-post-ra -disable-tail-duplicate --disable-branch-fold -disable-block-placement"
llc masked_xor.unison.mir -march=mipsel -mcpu=mips32 $flags -start-after livedebugvars -o masked_xor.s
```

## Contact

For any questions or issues on SecConCG contact:
[Rodothea Myrsini Tsoupidi](https://www.kth.se/profile/tsoupidi/) [<tsoupidi@kth.se>]


## License

SecConCG is licensed under the BSD3 license, see the [LICENSE.md](LICENSE.md) file
for details.
