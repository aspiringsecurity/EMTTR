import * as React from "react"
import {createRoot} from "react-dom/client"
import {App} from "./app"

const container = document.getElementById("app")
if (!container) {
    throw Error("couldn't find the html element to render into")
}
const root = createRoot(container)
root.render(<App />)