from itertools import *
from collections import deque
from colorist import ColorHSL
import copy
from more_itertools import *


def get(matrix, i):
    if i[0] < 0 or i[1] < 0:
        return None
    try:
        return matrix[i[0]][i[1]]
    except IndexError:
        return None


def connections(matrix, coord):
    i = coord[0]
    j = coord[1]
    match get(matrix, (i, j)):
        case "|":
            return [x for x in [(i + 1, j), (i - 1, j)] if get(matrix, x) != None]
        case "-":
            return [x for x in [(i, j + 1), (i, j - 1)] if get(matrix, x) != None]
        case "L":
            return [x for x in [(i - 1, j), (i, j + 1)] if get(matrix, x) != None]
        case "J":
            return [x for x in [(i - 1, j), (i, j - 1)] if get(matrix, x) != None]
        case "7":
            return [x for x in [(i + 1, j), (i, j - 1)] if get(matrix, x) != None]
        case "F":
            return [x for x in [(i + 1, j), (i, j + 1)] if get(matrix, x) != None]
        case ".":
            return []


def surrounding(matrix, coord):
    row = coord[0]
    col = coord[1]
    directions = []
    if get(matrix, (row + 1, col)) != None:
        directions.append((row + 1, col))
    if get(matrix, (row - 1, col)) != None:
        directions.append((row - 1, col))
    if get(matrix, (row, col + 1)) != None:
        directions.append((row, col + 1))
    if get(matrix, (row, col - 1)) != None:
        directions.append((row, col - 1))
    return directions


def traverse(matrix, i, j):
    visited = set()
    path = []

    connection = next((x for x in connections(matrix, i, j) if x != (i, j)), None)
    while connection != None:
        path.append(connection)
        visited.add(connection)
        connection = next(
            (
                x
                for x in connections(matrix, connection[0], connection[1])
                if x not in visited
            ),
            None,
        )
        if get(matrix, (connection[0], connection[1])) == "S":
            return path


def printMatrixSteps(matrix, steps: dict):
    matrix = list(map(lambda x: list(x), matrix))

    hue_multiplier = 360 / max(steps.values())

    for i, k in enumerate(steps.keys()):
        row = k[0]
        col = k[1]

        match matrix[row][col]:
            case "S":
                matrix[row][col] = "█"
            case "|":
                matrix[row][col] = "│"
            case "-":
                matrix[row][col] = "─"
            case "L":
                matrix[row][col] = "└"
            case "J":
                matrix[row][col] = "┘"
            case "7":
                matrix[row][col] = "┐"
            case "F":
                matrix[row][col] = "┌"

        matrix[k[0]][k[1]] = '\x1b[32m' + matrix[k[0]][k[1]] + "\x1b[0m"

    for line in matrix:
        print("".join(line))


if __name__ == "__main__":
    with open("input.txt") as file:
        matrix = list(map(lambda x: x.replace("\n", ""), iter(file.readlines())))
        steps = dict()
        for row, _ in enumerate(matrix):
            for col, _ in enumerate(matrix[row]):
                if get(matrix, (row, col)) == "S":
                    s_neighbors = surrounding(matrix, (row, col))
                    queue = deque()
                    steps[(row, col)] = 0
                    for i in (
                        x for x in s_neighbors if (row, col) in connections(matrix, x)
                    ):
                        steps[i] = 1
                        queue.append(i)

                    while len(queue) > 0:
                        cur = queue.popleft()
                        for i in (
                            x for x in connections(matrix, cur) if x not in steps.keys()
                        ):
                            steps[i] = steps[cur] + 1
                            queue.append(i)

                    print("part1:", max(steps.values()))

        outside = set()
        inside = set()
        for row, _ in enumerate(matrix):
            for col, _ in enumerate(matrix[row]):
                break

        printMatrixSteps(matrix, steps)
        # print('part1:',  max(steps.values()))
