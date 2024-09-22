import PF from 'pathfinding'

const finder = new PF.AStarFinder({ allowDiagonal: true, dontCrossCorners: true });

export function findPathImpl(mapMatrix) {
  return (sourcePosition) => {
    return (targetPosition) => {
      const grid = new PF.Grid(mapMatrix)
      return finder.findPath(sourcePosition.x, sourcePosition.y, targetPosition.x, targetPosition.y, grid)
    }
  }
}
