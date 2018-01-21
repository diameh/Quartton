import dee.quarto._

def allPositions = for {
  x <- 0 to 3
  y <- 0 to 3
} yield Pos(x, y)

//Board.allPositions.map(t => (t.a, t.b))




def allFigures = for {
  c <- Set(Light, Dark)
  h <- Set(Short, Tall)
  s <- Set(Round, Square)
  f <- Set(Filled, Hollow)
} yield Figure(c, h, s, f)
print(allPositions)
print(allFigures)
Figure(Light, Short, Round, Filled)
val allFilledBoard = (allPositions zip allFigures).toMap
val figuresInRow = allFilledBoard.values.toSet
// Johans example map.collect { case (Position(3, _), figure) => figure }

//another example: row.flatMap ( figure => Seq(figure.color, figure.whatever)
// .map(feature => feature) )

def getUniqueFeatures(row: Set[Figure])=
  for {
    fig <- row
    feature <- Set(fig.c, fig.h, fig.s, fig.f)
  } yield feature



def calculateScore(filledPositions: Map[Pos, Figure], lastAdded: Pos) = {
  score(getRow(filledPositions, lastAdded).toSet) + score(getColumn(filledPositions, lastAdded).toSet)
}

def getRow(filledPositions: Map[Pos, Figure], lastAdded: Pos) = {
  filledPositions.collect {
    case (Pos(lastAdded.row, _), figure) => figure
  }
}

def getColumn(filledPositions: Map[Pos, Figure], lastAdded: Pos) = {
  filledPositions.collect {
    case (Pos(_, lastAdded.col), figure) => figure }
}

def score(figures: Set[Figure]): Int = {
  if (figures.size == 4) 8 - getUniqueFeatures(figures).size
  else 0
}
val row = getRow(allFilledBoard, Pos(0,0))
val rowUni = getUniqueFeatures(row.toSet)
val col = getColumn(allFilledBoard, Pos(0,0))
val colUni = getUniqueFeatures(col.toSet)
val int = calculateScore(allFilledBoard, Pos(0,0))
val skoorRow = score(row.toSet)







