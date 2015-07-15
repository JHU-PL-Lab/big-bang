let deadCell = false
let liveCell = true

let makeCoordinate(column, row) =
  [column, row]

let getColumn(coordinate) =
  coordinate(0)

let getRow(coordinate) =
  coordinate(1)

let makeGrid(width, height) =
  ref grid = []
  repeat height times
    ref row = []
    repeat width times
      row = row + [deadCell]
    end
    grid = grid + row
  end
  grid

let traverseGrid(grid, function) =
  ref coordinate = makeCoordinate(0, 0)
  repeat gridHeight(grid) times
    repeat gridWidth(grid) times
      function(selectGrid(grid, coordinate), coordinate)
      coordinate = makeCoordinate(getColumn(coordinate) + 1, getRow(coordinate))
    end
    coordinate = makeCoordinate(getColumn(coordinate), getRow(coordinate) + 1)
  end

let cloneGrid(grid) =
  makeGrid(gridWidth(grid), gridHeight(grid))

let gridWidth(grid) =
  size(grid(0))

let gridHeight(grid) =
  size(grid)

let selectGrid(grid, coordinate) =
  grid(getColumn(coordinate))(getRow(coordinate))

let updateGrid(grid, coordinate, newValue) =
  grid(getColumn(coordinate))(getRow(coordinate)) = newValue

let isInGrid(grid, coordinate) =
  getColumn(coordinate) >= 0 and
  getRow(coordinate)    >= 0 and
  getColumn(coordinate) < gridHeight(grid) and
  getRow(coordinate)    < gridWidth(grid)

let neighbors(grid, coordinate) =
  let neighborsDifferences = [
    [-1, -1], [0, -1], [1, -1],
    [-1,  0],          [1,  0],
    [-1,  1], [0,  1], [1,  1],
  ]
  ref neighbors = []
  repeat for each neighborsDifference in neighborsDifferences
    neighbor = makeCoordinate (
      getColumn(coordinate) + neighborsDifference(0),
      getRow(coordinate) + neighborsDifference(1)
    )
    if isInGrid(grid, neighbor) then
      neighbors = neighbors + neighbor
    end
  end
  neighbors

let step(grid) =
  ref newGrid = cloneGrid(grid)
  traverseGrid(
    grid,
    function(cell, coordinate) =
      liveNeighbors = count(
        neighbors(grid, coordinate),
        function(neighbor) =
          return neighbor = livecell
      )
      if liveNeighbors < 2 then
        updateGrid(newGrid, coordinate, deadCell)
      else if liveNeighbors < 3
        updateGrid(newGrid, coordinate, cell)
      else if liveNeighbors < 4
        updateGrid(newGrid, coordinate, liveCell)
      else
        updateGrid(newGrid, coordinate, deadCell)
      end
  )
  newGrid
