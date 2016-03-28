var Composition3D;

 (function(Composition3D){

  /******************
   * 
   *  GEOMETRY
   * 
   *****************/
  /* Builder
     geo: Object returned by toJSONList.geoThreeJS
      buildFunc = obj$builder,
      buildArgs = params,
      materialIndex = obj$materialIndex,
      vcFun = obj$vertex.color.fun,
      fcFun = obj$face.color.fun,
      fmFun = obj$face.material.fun,
      bbox = obj$compute.bbox,
      fn = obj$compute.fn,
      vn = obj$compute.vn,
      dupvn = obj$remove.dupvn
  */
  var createGeometry = function( params ){
    
    var self = this; // Cache this
    
    // Call builder + args (this should return a THREEJS.Geometry object)
    //var geometry = Function.prototype.apply( params.buildFunc, params.buildArgs );
    var geometry = params.buildFunc.apply(params.buildFunc, params.buildArgs);
    
    
    // remove dup vertices
    ( params.dupvn && geometry.mergeVertices() );
    
    // compute vertex normals
    ( params.vn && geometry.computeVertexNormals() );
    
    // compute face normals
    if( params.fn ) geometry.computeFaceNormals() ;
    
    // compute bounding box
    ( params.bbox && geometry.computeBoundingBox() );
    
     // Vertex color function
    if( typeof params.vcFun == "function" ){
      geometry.colors = []; // Empty previous vector
      geometry.vertices.forEach( function(v,i){
        // Get color on each vertex
        geometry.colors.push(params.vcFun(v, geometry, i , params.extra, self.colorscale ));
      });
      // Flag need update
      geometry.colorsNeedUpdate = true;
      
      // Meshes use face color... 
      geometry.faces.forEach( function(f){
          f.vertexColors[0] =  geometry.colors[f.a];
          f.vertexColors[1] =  geometry.colors[f.b];
          f.vertexColors[2] =  geometry.colors[f.c];
      });
     
      // Flag face update
      geometry.elementsNeedUpdate = true;
    }
    
    // Set material index
    if(params.materialIndex !== 0){
      geometry.faces.forEach(function(f){ 
        f.materialIndex = params.materialIndex ;
      });
    }
    
    // Face color function
    if( typeof params.fcFun == "function"){
      geometry.faces.forEach( function(f,i){
        f.color = params.fcFun( f, geometry.vertices[f.a], geometry.vertices[f.b], geometry.vertices[f.c], geometry, i, params.extra, self.colorscale );
      });
    }
    
    // Face material function
    if( typeof params.fmFun == "function"){
      geometry.faces.forEach( function(f){
        f.materialIndex = params.fmFun( f, geometry.vertices[f.a], geometry.vertices[f.b], geometry.vertices[f.c], geometry, i, params.extra );
      });
    }
    return(geometry);
  };
  Composition3D.createGeometry = createGeometry;
  
    /******************
   * 
   *  HELPERS
   * 
   *****************/
   var createHelper = function( params, obj ){
     
     // For global -> obj is not needed
      if (params.type == "global")
        return( params.buildFunc.apply(params.buildFunc, params.buildArgs) );
      else if (params.type == "mesh"){
        params.buildArgs.push(obj);
        return( params.buildFunc.apply(params.buildFunc, params.buildArgs));
      }
      
   };
  
  /*******************************************************************/

  /*************
   *  
   * Object( MESH )
   *
   *************/  
   /* Builder
       ret <- list(
        geometries = geometries,
        material = ifelse(is.list(material), material[[1]], material),
        name = name,
        children = list(...)
     */
   var createMesh = function( params ){ 
     
      // Create all geometries
      var geoms = params.geometries.map( Composition3D.createGeometry,this );
      
      // Join all geometries
      for( var i = 1 ; i < geoms.length ; i++ ){
        
        // Merge points
        geoms[0].merge(geoms[i]);
      
        // Destroy merged geometry (Do we really need to do this or its something that merge does?)
        geoms[i].dispose();
      }
      
      // Create all materials
      var materials = params.material.map( function(m){ return( new m.buildFunc(m.buildArgs))});
      
      // IF more than 1, create facematerial
      if(materials.length > 1)
        materials = new THREE.MeshFaceMaterial( materials );
      else
        materials = materials[0];
        
      var mesh = new THREE.Mesh( geoms[0], materials );
      
      // set name
      mesh.name = params.name;
      
      // Add children to mesh
      if(params.children && ( params.children !== null ) ){
        //  Create other meshes
        var children = params.children.map( createMesh );
        //Function.prototype.apply(mesh.add, children);
        THREE.Mesh.prototype.add.apply(mesh, children);
     }
     
     // Add helpers
     if (params.helpers)
      THREE.Mesh.prototype.add.apply(mesh, params.helpers.map( function(x){ return(createHelper(x, mesh))} ) );
     
     return(mesh);
   };
   Composition3D.createMesh = createMesh;
   
  /*************
   *  
   * LIGHT
   *
   *************/
   var createLights = function( params ){
     //return(Function.prototype.apply( params.buildFunc, params.buildArgs ));
     return( params.map( function(x){
       return(x.buildFunc.apply(x.buildFunc, x.buildArgs));
     }));
   };
   Composition3D.createLights = createLights;
   
   /*******************************************************************/
   
   /******************
    * 
    * Composition
    * 
    * ***************/
    var Composition = ( function(){
      
      // Empty builder since initialize do not admit aniy params.
      function Composition( el, width, height ){
        
        var self = this; // Chache this
        
        // Save size
        this.height = height;
        this.width = width;
        
        // Save parent
        this.parentDiv = el;
        
        // Create empty scene
        this.scene = new THREE.Scene();
        
        // Create root object to make object creation/deletion easier
        this.rootObject = new THREE.Object3D();
        this.scene.add(this.rootObject);
        
        // Create root lights
        this.rootLight = new THREE.Object3D();
        this.scene.add(this.rootLight);
        
        // Default renderer
        this.renderer = new THREE.CanvasRenderer();
        this.GL=false;
        // Clear previous renderers
        this.parentDiv.innerHTML = "";
        this.parentDiv.appendChild(this.renderer.domElement);
        
        // Renderer size
        this.renderer.setSize( this.width, this.height);
        
        // Camera (empty)
        this.camera = new THREE.PerspectiveCamera(50, width/height, 0.1 , 1E3);
        this.setCameraPos(25,25,25);
        this.setCameraLookat(0,0,0);
        this.camera.updateProjectionMatrix();
        // Add camera to scene to plot "fixd" objects
        this.scene.add(this.camera);
        
        // Controls
        // this.controls = new THREE.OrbitControls(this.camera, el );
        this.controls = new THREE.MixedControls(this.camera, el );
        this.controls.addEventListener('change',function(){ self.render() });
        if (HTMLWidgets.shinyMode) this.controls.createHelperModal();
        
        // Colorscale
        this.colorscale = null; 
        this.legend = null;
        this.labels = null;
        
        
        // Clock (autostart)
        this.clock = new THREE.Clock(true);
        
        // Unassigned
        this.animationCb = null;
      }
      
      /********* Animation and rendering ******************/
      Composition.prototype.setAnimationCb = function( fun ){
        // Animation callback
        this.animationCb = fun;
      };
      
      Composition.prototype.render = function(){
        // var self = this;
        // requestAnimationFrame( function(){self.render()});
        
        this.renderer.render(this.scene, this.camera);
      };
      
      Composition.prototype.animate = function(){
         var self = this; //cache this here
         
        // Call ourselves in the next animation cycle
        requestAnimationFrame(function(){self.animate()}); 
        
        // Get clock delta
        var delta = this.clock.getDelta();
        
        // Update controls
        this.controls.update(delta);
        
        // Further animations
        if( this.animationCb !== null ) this.animationCb(delta, this);
      };

      Composition.prototype.resize = function(width, height){
        
        // Update size
        this.height = height;
        this.width = width;
        
        // Update renderer ad camera
        this.renderer.clear();
        this.renderer.setSize( width, height);
        this.camera.aspect = width / height; 
        this.camera.updateProjectionMatrix();

      };
      
      /************* CONTROLS ***********************/
      Composition.prototype.setControls = function (controlOpts){
        
        this.controls = new THREE.MixedControls(this.camera, el, controlOpts );
        this.controls.addEventListener('change',function(){ self.render() });
        this.controls.createHelperModal();
        
      };
      
      /************* RENDERER ***********************/
      Composition.prototype.setRenderer = function( name ){
        
        // Set renderer
        if(Detector.webgl && (name=="auto" || name=="webgl"))
        {
          this.renderer = new THREE.WebGLRenderer({antialias: true});
          this.GL = true;
        }
        else{
           this.renderer = new THREE.CanvasRenderer();
           this.GL=false;
        }
        
        // Renderer size
        this.renderer.setSize( this.width, this.height);
        
        // Add to parentDiv
        this.parentDiv.innerHTML = "";
        this.parentDiv.appendChild(this.renderer.domElement);
        if (HTMLWidgets.shinyMode) this.controls.createHelperModal();
        
      };
      
      Composition.prototype.setBgCoor = function(color){
        
        this.renderer.setClearColor( new THREE.Color(color) , 1 );
        
      };
      
      /*************** CAMERA **************************/
      Composition.prototype.setCameraProps = function(fov, near, far ){
        
        fov = typeof fov !== 'undefined' ? fov : this.camera.fov ;
        near = typeof near !== 'undefined' ? near : this.camera.near ;
        far = typeof far !== 'undefined' ? far : this.camera.far ;
        
        this.camera.fov = fov;
        this.camera.near = near;
        this.camera.far = far;
        
        this.camera.updateProjectionMatrix();
      };
      
      Composition.prototype.setCameraPos = function(x,y,z){
        
        x = typeof x !== 'undefined' ? x : this.camera.position.x ;
        y = typeof y !== 'undefined' ? y : this.camera.position.y ;
        z = typeof z !== 'undefined' ? z : this.camera.position.z ;
        
        this.camera.position.x = x;
        this.camera.position.y = y;
        this.camera.position.z = z;
        this.camera.updateProjectionMatrix();
      };
      
      Composition.prototype.setCameraLookat = function( x, y, z ){
        
        x = typeof x !== 'undefined' ? x : 0 ;
        y = typeof y !== 'undefined' ? y : 0 ;
        z = typeof z !== 'undefined' ? z : 0 ;
        
        this.camera.lookAt(  new THREE.Vector3(x,y,z) );
        this.camera.updateProjectionMatrix();
      };
      
      /** SET COLORSCALE **/
      Composition.prototype.setColorscale = function (params){
        
        // Create lut colorscale 
        this.colorscale = new THREE.Lut(params.colorMap, params.ncolors);
        
        // Set max/min values
        this.colorscale.setMax(params.max);
        this.colorscale.setMin(params.min);
        
        // Add legend
        if (params.legend){

          // Compute poisiton and dimension ( relative to camera )
          var nearYMax = Math.tan( (this.camera.fov/2) * (Math.PI/180) ) * 16 ;
          var nearXMax = nearYMax * this.camera.aspect ;
          
          var cameraPosition = {};
          cameraPosition.x = params.legend.position.x * 2 * nearXMax - nearXMax ;
          cameraPosition.y = params.legend.position.y * 2 * nearYMax - nearYMax ;
          cameraPosition.z = -16 ;
                                 
          var cameraDimension = {} ;
          cameraDimension.width = params.legend.dimensions.width * nearXMax * 2;
          cameraDimension.height = params.legend.dimensions.height * nearYMax * 2;
          
          // Update params
          params.legend.position = cameraPosition;
          params.legend.dimensions = cameraDimension;
          
          // Create legend
          this.legend = this.colorscale.setLegendOn( params.legend );
          // Add legend
          this.camera.add(this.legend);
          if (params.legend.labels){
            this.labels = this.colorscale.setLegendLabels( params.legend.labels );
            // Add title
            this.camera.add(this.labels.title);
            // Add ticks
            if (this.labels.ticks){
              for( var i = 0 ; i < Object.keys(this.labels.ticks).length ; i++ ){
                this.camera.add( this.labels.ticks[i]);
                this.camera.add( this.labels.lines[i]);
              }
            }
          }
        }
      };
      
      /****************************
       *  OBJECT (MESH) MANAGEMENT
       ***************************/
       
      //  objs from objects = lapply(obj$objects, toJSONList ),
      Composition.prototype.addJSONObjects = function( objs ){
        
        // Call object3D on each element
        var compObjs = objs.map( createMesh, this );
        
        // Add all to root
        THREE.Object3D.prototype.add.apply(this.rootObject, compObjs);
        
        //Function.prototype.apply(this.rootObject.add, compObjs);
        
        // Call render
        // this.render();
      };
      
      // Add regular Object3D
      Composition.prototype.addObject = function( obj ) {
        // Add single object to root object
        this.rootObject.add(obj);
        
        // Call render
        // this.render();
      };
      
      // Remove all objects in a scene
      Composition.prototype.clearObjects = function () {
        
        if(this.rootObject.children.length === 0 ) return ;
        
        // Remove rootObject from scene
        this.scene.remove(this.rootObject);
        
        // Traverse object disposing geometry and materials
        this.rootObject.traverse( function(m){
          if(m.geometry)
            m.geometry.dispose();
          if(m.material)
            m.material.dispose();
        });
        
        // Create new object
        this.rootObject = new THREE.Object3D();
        
        // Add to scene
        this.scene.add(this.rootObject);
        
        // Call render
        // this.render();
      };
      
      /*************************************/
      
      /****************************
       *  LIGHT MANAGEMENT
       ***************************/
       
       // lapply(obj$lights, toJSONList )
       Composition.prototype.addJSONLights = function( lights ){
         //Function.prototype.apply(this.rootLight.add, createLights(lights) );
         THREE.Object3D.prototype.add.apply(this.rootLight, createLights(lights));
       };
       
       // Regular light obj
       Composition.prototype.addLight = function( light ){
         this.rootLight.add(light);
       };
       
       // Clear
       Composition.prototype.clearLights = function(){
         
         if(this.rootLight.children.length === 0 ) return ;
         
         // Remove from scene ()
         this.scene.remove(this.rootLight);
         
         // Update
         this.rootLight = new THREE.Object3D();
         
         // Add to scene
         this.scene.add(this.rootLight);
       };
       
       /*******************************
        *  HELPERS 
        * **************************/
      Composition.prototype.addJSONHelpers = function (helpers){
          var self = this;
          THREE.Mesh.prototype.add.apply(this.rootObject, helpers.map( function(x){ return(createHelper(x, self.rootObject)) } ) );
      };
      
      return Composition;
    })();
    Composition3D.Composition = Composition;
    
   /*******************************************************************/
 })(Composition3D || (Composition3D = {}));