import { basicSetup, EditorView } from "codemirror"
import { javascript } from "@codemirror/lang-javascript"
import { EditorState, Compartment } from "@codemirror/state"

export function createEditorImpl(contentsChangeCallback) {
  return () => {
    const initialDocumentText = ""
    const parent = document.createElement("div")
    parent.id = "editor"
    const language = new Compartment
    const tabSize = new Compartment

    const updateListener = EditorView.updateListener.of(update => {
      if (update.docChanged) {
        contentsChangeCallback(update.state.doc.toString())()
      }
    })

    new EditorView({
      doc: initialDocumentText,
      extensions: [
        basicSetup,
        language.of(javascript()),
        tabSize.of(EditorState.tabSize.of(4)),
        updateListener,
      ],
      parent,
    })

    return parent
  }
}

