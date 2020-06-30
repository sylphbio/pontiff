(module bin.pontiff ()

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.process-context)

(import tabulae)
(import tabulae.monad)
(import ix)

(import (prefix state state:))
(import (prefix argv argv:))
(import (prefix command command:))

; nice future things...
; * __VERSION__ __LINE__ etc replacements. possibly by user compiler pass? pontiff language extensions lol
; * our own import files, so we can fully parallelize compilation
;   maybe a declare that indicates no macros are exported...
;   complicated tho, we need to parse existing import files to resolve reexports
; * tests lol
; * vcs behind a generic interface
; * centralize hardcoded values in some module
; * command.repl, command.release
; * impl resolver lol
; * build embedded. basically just set -DC_EMBEDDED. examples just have a c file that includes chicken.h and pass it to csc too
; * cross-compilation. do not fuck with this until pontiff manages its own compilers tho
; * some kind of trivial eggification? say... I could split the module sorting out into like command.build.module
;   and then use that in scripts. and then to make a pontiff project an egg I don't need to trash my directory structure
;   I can just have a custom build that cats the files together in the right order and passes to csc
;   don't make it too complicated, this is a convenience hack not a proper feature

; and aside from that I need to do ix/json conversions and rewrite the sbml parser for viv
; XXX I'm a genius the "safe" way to do pontiff upgrades is write functional mappings for pontiff files across versions
; and then just depclean the fucking project whenever the pontiff file version and pontiff version don't match lmao
; XXX should I just like, add a prologue that forces a proper repo string in dynamic builds?
; the fucky thing here is... if I set it for pontiff, can pontiff use its own extensions and properly set those of its build?
; hmm I... guess it only matters for dynamic executables, and I *think* I only "use" it for chicken-install calls
; so if I bake in CHICKEN_REPOSITORY_PATH in pontiff it should work fine and state:env ignores it for PONTIFF_LINK_PATH anyway
; XXX TODO I had a really good idea once I have my prologue system set up that I can also have an epilogue
; specifically to make it so an exe exports but does not call main, and we call main in the epilogue
; XXX I can take over lmdb-ht, the setup script is trivial and just does a bunch of legwork to verify lmdb is installed
; the only feature I need is a way to specify library dependencies. but first need to fix pontiff dep ordering
; also figure out when to link and what order to use...
; dyn we just link our own, static lib we link nothing obv, static exe we need to link libs for all deps
; order matters but repeats are allowed so could just put them in dep order. or hm I guess deduping should work
; XXX prolly add optional cflag/ldflag things to the conf
; XXX there's an ugly bug with -inline-global where it pulls like an elf object into module registration?
; idk make minimal test case. at least in 5.2.0. hilariously in master it just segfaults
; XXX TODO when I break out vcs, use the git egg. need to link git2
; XXX making tests parallelizable would be nice

(define (main)
  (state:init)

  (define ret (do/m <either>
    (args <- (argv:process (command-line-arguments)))
    (command:dispatch args)))

  (when (left? ret)
        (printf (<> (cdadr ret) "\n"))
        (exit (caadr ret)))

  (exit 0))

(main)

)
