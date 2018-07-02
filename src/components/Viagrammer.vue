<template>
    <ViagrammerElm></ViagrammerElm>
</template>

<script>
import * as ElmComponent from "./ElmBridge"

export default {
  name: 'Viagrammer',
  components: {
    'ViagrammerElm': ElmComponent(require('./elm/src/Main.elm').Main)
  }
}
</script>

<style>
#app {
}
</style>
