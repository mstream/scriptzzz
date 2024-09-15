import { Application, Assets, Container, Graphics, GraphicsContext, Sprite, Text, TextStyle } from 'pixi.js';

const context = {
  application: null,
  entities: {},
  textures: {
    entity: null,
  }
}

export function createCanvasImpl(parentSelector) {
  return async () => {
    const parent = document.querySelector(parentSelector)
    const app = new Application();
    await app.init({ width: 400, height: 400 });
    parent.appendChild(app.canvas);
    context.application = app

    const entityTexture = await Assets.load('entity.png')
    context.textures.entity = entityTexture
  }
}

export function createEntityImpl(id, position) {
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

export function updateEntityPositionImpl(id, position) {
  return () => {
    const entity = context.entities[id]
    if (!entity) {
      throw Error(`Entity '${id}' not found.`)
    }
  }
}

export function execJsImpl(code) {
  return () => {
    const fn = new Function(code)
    return JSON.stringify(fn())
  }
}
