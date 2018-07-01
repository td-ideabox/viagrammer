<template>
  <div id="app">
    <img src="./assets/logo.png">
    <router-view/>
    <Viagrammer></Viagrammer>
  </div>
</template>

<script>
import * as ElmComponent from "./elm"

export default {
  name: 'App',
  components: {
    'Viagrammer': ElmComponent(require('./elm/src/Main.elm').Main)
  }
}
</script>

<style>
#app {
}
</style>
