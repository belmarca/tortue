## How to profile

./tortue-exe +RTS -T -N -hc -RTS
hp2ps tortue-exe.hpzz
ps2pdf tortue-exe.ps tortue-exe.pdf

## Shrinking HS rvm even more

### Renaming imported identifiers

Because these identifiers are imported, they can't be renamed by the minify script.
Either extend the minify script or manually create binding for these identifiers.

pure (21)
unsafePerformIO (4)
reverse (3)
flip (4)
foldrM (2)

### Whitespace

These regexes can be applied manually on the minified VM to reduce the total size.

) ( -> )(
(\w)\s+\( -> $1(
\)\s+(\w) -> )$1
\)\s+\( -> )(
(\w)\s+\{ -> $1{
\}\s+(\w) -> }$1
\}\s+\{ -> }{
 $  -> $
([^A-Z]) \. -> $1.
;
