// External Imports ------------------------------------------------------------
const commandLineArgs = require('command-line-args')
const fetch = require('node-fetch')
const fs = require('fs')

// Local Imports ---------------------------------------------------------------
const { Elm } = require('./Main.elm')

// Elm App ---------------------------------------------------------------------
const app = Elm.Main.init()
const send = input => app.ports.fromJs.send({ day, part, year, input })

app.ports.fromElm.subscribe(result => {
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

      send(input)
    })
    .catch(console.log)
}
