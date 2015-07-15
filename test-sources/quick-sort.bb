let quickSort(elements) =
  let partition(elements, pivot) =
    ref lessThenPivot = []
    ref greaterThanPivor = []
    repeat for each element in elements
      if element < pivot
        lessThenPivot = lessThenPivot + [element]
      else
        greaterThenPivot = greaterThenPivot + [element]
      end
    end
  in
  if elements = [] then
    elements
  else
    let pivot = elements[0]
    let rest  = slice(elements, 1, -1)
    let partitions = partition(rest, pivot)
    partitions(0) + [pivot] + partitions(1)
  end

quickSort([5,3,6,1])
