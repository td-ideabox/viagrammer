import Vue from 'vue'
import Router from 'vue-router'
import Viagrammer from '@/components/Viagrammer'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      name: 'Viagrammer',
      component: Viagrammer
    }
  ]
})
