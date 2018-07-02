<template>
  <div>
    <ViagrammerElm></ViagrammerElm>
  </div>
</template>

<script>
import * as ElmComponent from "./ElmBridge"
import Viz from "viz.js"
import {Module, render} from "viz.js/full.render.js"
export default {
  name: 'Viagrammer',
  methods: {
    vizJson: (e) => {
      let v = new Viz( {Module, render});
      v.renderJSONObject("digraph { a -> b; b->c; c -> a;}").then(result => {
        console.log(result)
        this.vizJson = result
      }).catch(err => {
        this.vizJson = err
      })
    }
  },
  components: {
    'ViagrammerElm': ElmComponent(require('./elm/src/Main.elm').Main)
  }
}
</script>

<style>
#app {
}
</style>
