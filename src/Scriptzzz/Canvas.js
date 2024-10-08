import { Application, Assets, Container, Graphics, GraphicsContext, Sprite, Text, TextStyle } from 'pixi.js';

const minCellDimension = 3

const context = {
  application: null,
  entities: {},
  grid: {
    cellSize: {
      height: null,
      width: null,
    },
    height: null,
    pixelHeight: null,
    pixelWidth: null,
    width: null,
  },
  textures: {
    entity: null,
  }
}

function gridToCellX(x) {
  return x * context.grid.cellSize.width
}

function gridToCellY(y) {
  return y * context.grid.cellSize.height
}

function checkApplicationInitialized() {
  if (!context.application) {
    throw Error('Application uninitialized')
  }
  if (!context.grid.width || !context.grid.height) {
    throw Error('Grid uninitialized')
  }

  const cellWidth = context.grid.cellSize.width
  const cellHeight = context.grid.cellSize.height

  if (cellWidth < minCellDimension || cellHeight < minCellDimension) {
    throw Error(`Grid cell to small: (${cellWidth},${cellHeight})`)
  }
}

function createGrid() {
  checkApplicationInitialized()
  const graphics = new Graphics()
  for (let j = 0; j < context.grid.height; j++) {
    for (let i = 0; i < context.grid.width; i++) {
      const x = 1 + i * context.grid.cellSize.width
      const y = 1 + j * context.grid.cellSize.height
      graphics.rect(x, y, context.grid.cellSize.width - 2, context.grid.cellSize.height - 2)
      graphics.stroke({ alpha: 0.2, color: 0xffffff });
      graphics.fill({ alpha: 0.1, color: 0xffffff })
    }
  }
  return graphics
}

async function initializeApplication({ gridHeight, gridWidth, pixelHeight, pixelWidth }) {
  context.grid.width = gridWidth;
  context.grid.height = gridHeight;
  context.grid.pixelWidth = pixelWidth;
  context.grid.pixelHeight = pixelHeight;
  context.grid.cellSize.width = pixelWidth / gridWidth;
  context.grid.cellSize.height = pixelHeight / gridHeight;
  const app = new Application();
  context.application = app
  await app.init({ width: pixelWidth, height: pixelHeight });
  const entityTexture = await Assets.load('entity.png')
  context.textures.entity = entityTexture
  app.stage.addChild(createGrid({ gridHeight, gridWidth, pixelHeight, pixelWidth }))
  return app
}

export function createCanvasImpl(gridWidth) {
  return (gridHeight) => {
    return (pixelWidth) => {
      return (pixelHeight) => {
        return (canvasInitializedCallback) => {
          return () => {
            const parent = document.createElement("div")
            parent.id = "canvas"
            initializeApplication({ gridHeight, gridWidth, pixelHeight, pixelWidth }).then(app => {
              parent.appendChild(app.canvas)
              canvasInitializedCallback()
            })
            return parent
          }
        }
      }
    }
  }
}

export function destroyEntityImpl(id) {
  return () => {
    checkApplicationInitialized()
    const entity = context.entities[id]

    if (!entity) {
      throw Error(`Entity '${id}' not found.`)
    }

    context.application.stage.removeChild(entity)
  }
}

export function createEntityImpl(id) {
  return (entityType) => {
    return (position) => {
      return () => {
        checkApplicationInitialized()

        if (context.entities[id]) {
          throw Error(`Entity '${id}' already exists.`)
        }

        const sprite = new Sprite(context.textures.entity)
        //sprite.anchor.set(0.5)
        sprite.x = 0
        sprite.y = 0

        const label = new Text({ style: new TextStyle({ align: 'center', fill: 'white', fontSize: 10 }), text: id })
        //label.anchor.set(0.5)
        label.x = sprite.x
        label.y = sprite.y + 20

        const entity = new Container()
        entity.addChild(sprite)
        entity.addChild(label)

        entity.x = gridToCellX(position.x)
        entity.y = gridToCellY(position.y)


        context.application.stage.addChild(entity)
        context.entities[id] = entity
      }
    }
  }
}

export function updateEntityPositionImpl(id) {
  return (position) => {
    return () => {
      checkApplicationInitialized()
      const entity = context.entities[id]

      if (!entity) {
        throw Error(`Entity '${id}' not found.`)
      }

      entity.x = gridToCellX(position.x)
      entity.y = gridToCellY(position.y)
    }
  }
}


