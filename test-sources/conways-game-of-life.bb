let deadCell = false
let liveCell = true

fun makeCoordinate(column, row) =
  [column, row]
end

fun getColumn(coordinate) =
  coordinate(0)
end

fun getRow(coordinate) =
  coordinate(1)
end

fun makeGrid(width, height) =
  ref grid = []
  repeat height times
    ref row = []
    repeat width times
      row = row + [deadCell]
    end
    grid = grid + row
  end
  grid
end

fun traverseGrid(grid, function) =
  ref coordinate = makeCoordinate(0, 0)
  repeat gridHeight(grid) times
    repeat gridWidth(grid) times
      function(selectGrid(grid, coordinate), coordinate)
      coordinate = makeCoordinate(getColumn(coordinate) + 1, getRow(coordinate))
    end
    coordinate = makeCoordinate(getColumn(coordinate), getRow(coordinate) + 1)
  end
end

fun cloneGrid(grid) =
  makeGrid(gridWidth(grid), gridHeight(grid))
end

fun gridWidth(grid) =
  size(grid(0))
end

fun gridHeight(grid) =
  size(grid)
end

fun selectGrid(grid, coordinate) =
  grid(getColumn(coordinate))(getRow(coordinate))
end

fun updateGrid(grid, coordinate, newValue) =
  grid(getColumn(coordinate))(getRow(coordinate)) = newValue
end

fun inGrid?(grid, coordinate) =
  getColumn(coordinate) >= 0 and
  getRow(coordinate)    >= 0 and
  getColumn(coordinate) < gridHeight(grid) and
  getRow(coordinate)    < gridWidth(grid)
end

fun neighbors(grid, coordinate) =
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
    if inGrid?(grid, neighbor) then
      neighbors = neighbors + neighbor
    end
  end
  neighbors
end

fun step(grid) =
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
      else if liveNeighbors < 3 then
        updateGrid(newGrid, coordinate, cell)
      else if liveNeighbors < 4 then
        updateGrid(newGrid, coordinate, liveCell)
      else
        updateGrid(newGrid, coordinate, deadCell)
      end
  )
  newGrid
end
