#!/usr/bin/env nu

# check if package-list.json and bad.txt has any match.
# time: O(mn) where m is the number of lines in bad.txt and n is the number
# of entries of package-list.json/dependencies.
let bad: record = open --raw bad.txt
 | lines
 | each { |l| $l | parse "{name}@{version}" | get 0 }
let deps: table = open package-list.json
let offenses: int = $deps.dependencies | items { |k, v|
 mut c = 0
 for row in $bad {
  if $row.name == $k and $row.version == $v.version {
   print $row
   $c += 1
  }
 }
 return $c
} | math sum
print ("Offenses: " + ($offenses | to text))
