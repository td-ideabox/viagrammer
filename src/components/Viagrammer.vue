<template>
  <div>
    <button v-on:click="vizJson">vizify</button>
    <ViagrammerElm :ports="setupPorts"></ViagrammerElm>
  
  </div>

</template>

<script>
import * as ElmComponent from "./ElmBridge"
import Viz from "viz.js"
import {Module, render} from "viz.js/full.render.js"
export default {
  name: 'Viagrammer',
  methods: {
    vizJson: (dot, sendData) => {
      var that = this; 
      let v = new Viz( {Module, render});
      v.renderJSONObject(dot).then(result => {
        if(result) {
          sendData(JSON.stringify(result))  
        }
      }).catch(err => {
        console.log(err) 
      })
    },
    setupPorts: function(ports) {
      var that = this; 
      ports.sendDot.subscribe(function(message) {
         let d = that.vizJson(message, (data) => {ports.layoutData.send(data)})
      })
      this.ports = ports
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
