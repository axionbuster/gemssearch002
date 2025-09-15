#!/usr/bin/env nu
for case in (ls cases/*.in) {
 let name: string = $case.name
 let out = $name | path parse | upsert extension {'out'} | path join
 open --raw $name | stack run | save -f $out
}
