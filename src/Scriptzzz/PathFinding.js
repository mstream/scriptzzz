import PF from 'pathfinding'

const finder = new PF.AStarFinder({ allowDiagonal: true, dontCrossCorners: true });

export function findPathImpl(obstacleMatrix) {
  return (sourcePosition) => {
    return (targetPosition) => {
      const grid = new PF.Grid(obstacleMatrix)
      return finder.findPath(sourcePosition.x, sourcePosition.y, targetPosition.x, targetPosition.y, grid)
    }
  }
}
