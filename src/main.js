// External Imports ------------------------------------------------------------
const commandLineArgs = require('command-line-args')
const fetch = require('node-fetch')
const fs = require('fs')

let startTime = process.hrtime()

// Local Imports ---------------------------------------------------------------
const { Elm } = require('./Main.elm')

// Elm App ---------------------------------------------------------------------
const app = Elm.Main.init()
const send = input => app.ports.fromJs.send({ day, part, year, input })

app.ports.fromElm.subscribe(result => {
  const endTime = process.hrtime(startTime)

  console.info('Execution time: %ds %dms', endTime[0], endTime[1] / 1000000)
  console.log(`The result for Advent of Code ${year} day ${day} part ${part} is: ${result}`)
})

// Get Input -------------------------------------------------------------------
const { day, part, year } = commandLineArgs([
  { name: 'day', alias: 'd', type: Number },
  { name: 'part', alias: 'p', type: Number },
  { name: 'year', alias: 'y', type: Number }
])

try {
  const input = require(`../data/${year}/${day}.json`)

  startTime = process.hrtime()
  send(input)
} catch (e) {
  const session = require('../data/session.json')
  const requestParams = {
    method: 'GET',
    headers: { Cookie: `session=${session}` }
  }

  fetch(`https://adventofcode.com/${year}/day/${day}/input`, requestParams)
    .then(res => res.text())
    .then(input => {
      fs.writeFile(`data/${year}/${day}.json`, JSON.stringify(input), err => {
        if (err) console.log(err)
      })

      startTime = process.hrtime()
      send(input)
    })
    .catch(console.log)
}
