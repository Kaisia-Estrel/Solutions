defmodule Day1 do

  def part1 do
    File.read!("./input.txt")
      |> String.split("\n")
      |> Enum.flat_map(fn str -> 
        digits = String.codepoints(str) |> Enum.filter(&(String.match? &1, ~r/[0-9]/)) 
        case digits do 
          [] -> []
          xs -> [String.to_integer((List.first xs) <> (List.last xs))]
        end
      end)
      |> Enum.sum
  end

  def getFirstAndLast([]), do: []
  def getFirstAndLast(xs), do: [String.to_integer(List.first(xs) <> List.last(xs))]

  def splitByDigit([]), do: []
  def splitByDigit(["o" , "n" , "e" | _]), do: ["1"]
  def splitByDigit(["t" , "w" , "o" | _]), do: ["2"]
  def splitByDigit(["t" , "h" , "r" , "e" , "e" | _]), do: ["3"]
  def splitByDigit(["f" , "o" , "u" , "r" | _]), do: ["4"]
  def splitByDigit(["f" , "i" , "v" , "e" | _]), do: ["5"]
  def splitByDigit(["s" , "i" , "x" | _]), do: ["6"]
  def splitByDigit(["s" , "e" , "v" , "e" , "n" | _]), do: ["7"]
  def splitByDigit(["e" , "i" , "g" , "h" , "t" | _]), do: ["8"]
  def splitByDigit(["n" , "i" , "n" , "e" | _]), do: ["9"]
  def splitByDigit([x | _]) do
    if String.match?(x, ~r/[0-9]/) do
      [x]
    else 
      []
    end
  end

  def tails([]), do: []
  def tails([x | xs]), do: [[x | xs] | tails(xs)]

  def part2 do
    File.read!("./input.txt")
      |> String.split("\n")
      |> Enum.map(fn x -> 
        String.graphemes(x)
          |> tails
          |> Enum.flat_map(&splitByDigit/1)
      end)
      |> Enum.flat_map(&getFirstAndLast/1)
      |> Enum.sum
  end
end
