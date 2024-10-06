import { Application, Assets, Container, Graphics, GraphicsContext, Sprite, Text, TextStyle } from 'pixi.js';

const context = {
  application: null,
  entities: {},
  textures: {
    entity: null,
  }
}

async function initializeApplication() {
  const app = new Application();
  await app.init({ width: 400, height: 400 });
  const entityTexture = await Assets.load('entity.png')
  context.application = app
  context.textures.entity = entityTexture
  return app
}

export function createCanvasImpl(canvasInitializedCallback) {
  return () => {
    const parent = document.createElement("div")
    parent.id = "canvas"
    initializeApplication().then(app => {
      parent.appendChild(app.canvas)
      canvasInitializedCallback()
    })
    return parent
  }
}

export function createEntityImpl(id) {
  return (position) => {
    return () => {
      if (context.entities[id]) {
        throw Error(`Entity '${id}' already exists.`)
      }

      const sprite = new Sprite(context.textures.entity)
      sprite.anchor.set(0.5)
      sprite.x = position.x
      sprite.y = position.y

      const label = new Text({ style: new TextStyle({ align: 'center', fill: 'white', fontSize: 10 }), text: id })
      label.anchor.set(0.5)
      label.x = position.x
      label.y = position.y + 20

      const entity = new Container()
      entity.addChild(sprite)
      entity.addChild(label)

      context.application.stage.addChild(entity)
      context.entities[id] = entity
    }
  }
}

export function updateEntityPositionImpl(id) {
  return (position) => {
    return () => {
      const entity = context.entities[id]
      if (!entity) {
        throw Error(`Entity '${id}' not found.`)
      }
    }
  }
}


