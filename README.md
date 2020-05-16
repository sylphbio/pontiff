# pontiff
*"omnia publica privataque sacra pontificis scitis subiecit"*  
a haskell-style build system for chicken scheme

pontiff makes one core assumption/imposition: one module is one file is one compilation unit  
from this basis, it resolves a build order automatically, builds in parallel, fetches and sandboxes all dependencies  
eventually pontiff will allow users to specify dependency version constraints  
it will also control the compiler and runtime, allowing reproducible builds across platforms and for all time

pontiff is presently under internal development  
until its featureset and interface are locked down, pull requests will be politely declined  
feature requests made via issue will be considered. bug reports are appreciated  
my goal is to develop pontiff in a backward-compatible manner, with functional migrations across pontiff file versions  
but if I break your project feel free to yell at me (alicemaz) in #chicken on freenode

presently, pontiff works on linux (and probably bsd?) and requires clang lld and git  
I'll support gcc and gnu ld soon. I'll support svn maybe soon-ish. I'd like to support pijul if only for aesthetic reasons  
I will probably support windows grudgingly someday but not any time in the near future  
the bootstrapping script won't work on archlinux unless you alias chicken-csi to csi but it's an easy fix

dependencies must be specified longform via the repository and directory objects  
but this shouldn't be much of a problem since no one except me has ever released a pontiff project  
fairly soon I will have a barebones resolver so you can specify a symbol that will be transformed to a repo by some means  
eventually I'll have a way to publish versioned tarballs and allow semver patterns (or something like that) to pull them down

to get started, run bootstrap.scm. it will build a minimum viable pontiff, then use that to build pontiff properly  
it invokes `chicken-install -s` so be careful if that's something you dislike  
from there, put the resulting (statically linked) pontiff binary in PATH somewhere  
`pontiff new [project name]` will create a hello world project with one executable  
`pontiff build` and `pontiff run` do what you expect  
I will have more kawaii documentation on the pontiff.ix file eventually  
but a fairly readable type spec of it can be found in src/prototype/file.scm

remember, a file is a module is a compilation unit  
more specifically, a module's path uniquely determines its name:  
src/a/b/c.scm *must* be a file containing one and only one module, named a.b.c  
there are some things we don't support on purpose (like functors; expect a typeclass system in tabulae sooner or later!)  
also it is very important that no macros mess with import lists, as we *must* parse them *before* compilation to sort modules

this is our first nontrivial software release, with more to come!  
sylph will mostly be built on scheme haskell and agda and we want to contribute back whatever we reasonably can  
here's to the future
