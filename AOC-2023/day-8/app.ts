/* eslint-disable @typescript-eslint/no-non-null-assertion */
/* eslint-disable generator-star-spacing */
/* eslint-disable @typescript-eslint/space-before-function-paren */
import { open } from 'node:fs/promises'

type Direction = 'L' | 'R'

interface Input {
  instructions: Direction[]
  paths: Map<string, Map<Direction, string>>
}

const input: Promise<Input> = open('./input.txt').then(async (x) => {
  const lines = []

  for await (const line of x.readLines()) {
    lines.push(line.trim())
  }

  const paths = new Map<string, Map<Direction, string>>()

  lines.slice(2).forEach((x) => {
    const split1 = x.replace(/[^0-9A-Z]/g, ' ').split(/[ ]+/)
    paths.set(
      split1[0],
      new Map([
        ['L', split1[1]],
        ['R', split1[2]]
      ])
    )
  })

  return {
    instructions: lines[0].split('').map((x) => {
      if (x !== 'L' && x !== 'R') {
        throw new Error('instructions did not follow "LR" format')
      } else return x
    }),
    paths
  }
})

function* cycle<T>(xs: Iterable<T>): Generator<T> {
  while (true) {
    for (const i of xs) {
      yield i
    }
  }
}

async function part1(): Promise<void> {
  const nodes = await input

  let steps = 0
  const directions = cycle(nodes.instructions)
  for (
    let key = 'AAA';
    key !== 'ZZZ';
    key = nodes.paths.get(key)!.get(directions.next().value)!
  ) {
    steps++
  }
  console.log(`part1: ${steps}`)
}

function gcd(a: number, b: number): number {
  if (a % b > 0) {
    return gcd(b, a % b)
  } else {
    return b
  }
}
function lcm(a: number, b: number): number {
  return (a * b) / gcd(a, b)
}

async function part2(): Promise<void> {
  const nodes = await input

  const keys = [...nodes.paths.keys()].filter((x) => x.endsWith('A'))

  const keyLoopLengths = keys.map((key) => {
    const directions = cycle(nodes.instructions)
    let steps = 0
    for (
      ;
      !key.endsWith('Z');
      key = nodes.paths.get(key)!.get(directions.next().value)!
    ) {
      steps++
    }
    return steps
  })

  console.log(`part2: ${keyLoopLengths.reduce(lcm)}`)
}

void part1()
void part2()
