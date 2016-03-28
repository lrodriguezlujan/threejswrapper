/* compositionThree.js
* Javascript functions that support a threejs general purpose composition
*/
  HTMLWidgets.widget(
    {

      name: "threejswrapper",
      type: "output",

      initialize: function(el, width, height)
      {
        // Create empty composition
        var comp = new Composition3D.Composition(el, width, height);

        // Call animate ( update controls , and render if needed )
        comp.animate();

        // Composition as instance
        return(comp);
      },

      resize: function(el, width, height, instance)
      {
        // Just call resize fn
        instance.resize(width,height);

        // Call render (update)
        instance.render();
      },

      /*
            objects = lapply(obj$objects, toJSONList ),
            objectsAppend = obj$objectsAppend,
            lights = lapply(obj$lights, toJSONList ),
            lightsAppend = obj$lightsAppend,
            camera = obj$camera,
                  fov = camera.fov,
                  far = camera.far,
                  near = camera.near,
                  pos = camera.pos,
                  look = camera.lookat
            bgColor = obj$bgColor,
            renderer = obj$renderer,
            animateFun = obj$animateFun
       */
      renderValue: function(el, x, instance)
      {
        // Set camera props
        if(x.camera){
          // Props
          if(x.camera.fov || x.camera.far || x.camera.near)
            instance.setCameraProps(x.camera.fov, x.camera.near, x.camera.far);
          // Pos
          if(x.camera.pos)
            instance.setCameraPos(x.camera.pos[0], x.camera.pos[1], x.camera.pos[2]);
          // Look at
          if(x.camera.look)
              instance.setCameraLookat(x.camera.look[0], x.camera.look[1], x.camera.look[2]);
        }

        // Controls
        if( x.controls ) instance.setControls( x.controls );

        // BgColor
        if( x.bgColor ) instance.setBgCoor( x.bgColor );

        // Renderer
        if( x.renderer ) instance.setRenderer( x.renderer );

        // animateFun
        if( x.animateFun ) instance.setAnimationCb( x.animateFun );

        // Colorscale (before objs)
        if( x.colorscale ){
          instance.setColorscale(x.colorscale);
        }

        // Objects
        if( x.objects ){
          // Clear objects
          if( ! x.objectsAppend ) instance.clearObjects();
          instance.addJSONObjects(x.objects);
        }

        // Lights
        if( x.lights ){
          // Clear objects
          if( ! x.lightsAppend ) instance.clearLights();
          instance.addJSONLights(x.lights);
        }

        // Helpers
        if( x.helpers ){
          instance.addJSONHelpers(x.helpers);
        }



        // Call animate: NOT NEEDED!
        // instance.animate();

        // Render
        instance.render();
      }

});
