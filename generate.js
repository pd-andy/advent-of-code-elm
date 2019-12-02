const commandLineArgs = require('command-line-args')
const fs = require('fs')
const options = commandLineArgs([
  { name: 'day', alias: 'd', type: Number },
  { name: 'year', alias: 'y', type: Number }
])

const day = String(options.day).padStart(2, '0')
const year = options.year || 2019

const template = `module AdventOfCode${year}.Day${day} exposing
  ( part1
  , part2
  )

-- Imports ---------------------------------------------------------------------
import Utils

-- Functions -------------------------------------------------------------------
--
solveWith : function -> String -> Float
solveWith solver input =
  Debug.todo "Solve day ${day}."

-- Solvers ---------------------------------------------------------------------
part1 : String -> Float
part1 =
  solveWith never

part2 : String -> Float
part2 =
  solveWith never
`

// -----------------------------------------------------------------------------
fs.writeFile(`src/AdventOfCode${year}/Day${day}.elm`, template, err => {
  if (err) throw err
})

fs.readFile('src/Main.elm', (err, data) => {
  if (err) throw err

  const code = data.toString().split('\n')

  const injectionImportIndex = code.findIndex(line => line.includes(`<< INJECT ${year} IMPORT >>`))
  const injectionImport = [
    `import AdventOfCode${year}.Day${day}`
  ]

  const injectionCodeIndex = code.findIndex(line => line.includes(`<< INJECT ${year} SOLUTION >>`))
  const injectionCode = [
    `    , Tuple.pair ( ${options.day}, 1, ${year} ) AdventOfCode${year}.Day${day}.part1`,
    `    , Tuple.pair ( ${options.day}, 2, ${year} ) AdventOfCode${year}.Day${day}.part2`
  ]

  code.splice(injectionImportIndex, 0, ...injectionImport)
  code.splice(injectionCodeIndex + 1, 0, ...injectionCode)

  fs.writeFile('src/Main.elm', code.join('\n'), err => {
    if (err) throw err
  })
})
