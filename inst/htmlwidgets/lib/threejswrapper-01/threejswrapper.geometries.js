Composition3D.geometries = {};

Composition3D.geometries.meshWrapper = function( vertices, faces, vcolors ){
  
  // Create empty geometry
  var geo = new THREE.Geometry();
  
  // Add vertices
  
  // Long to D3 and then create vector3D for each component
  //geo.vertices = HTMLWidgets.dataframeToD3(vertices)
  geo.vertices = HTMLWidgets.dataframeToD3(vertices)
    .map(function(v){ return( new THREE.Vector3(v.x, v.y, v.z) ) });
  
  // Repeat the same process for the faces
  // geo.faces = HTMLWidgets.dataframeToD3(faces)
  geo.faces = HTMLWidgets.dataframeToD3(faces)
    .map(function(f){ return( new THREE.Face3( f.a , f.b , f.c ) ) });
    
  // If vcolors is not null, set colors
  if( vcolors !== null ){
    geo.colors = vcolors.map( function(c){ return( new Three.Color(c) ) } );
  }
  
  return ( geo );
};

Composition3D.geometries.sphereWrapper = function( radius, position, rotation, wsegs, hsegs, phis, phil, thetas, thetae){
  
  // Create geometry
  var geo = new THREE.SphereGeometry(radius, wsegs, hsegs, phis, phil, thetas, thetae );
  
  // Move
  // NOTE: geometry.translate exists from three.js r72 ...
  geo.applyMatrix ( new THREE.Matrix4().makeTranslation(position[0],position[1],position[2]) );
  
  // Rotate
  //var tmpQ  = new THREE.Quaternion( rotation[0], rotation[1], rotation[2], 1 ).normalize();
  geo.applyMatrix ( new THREE.Matrix4().makeRotationX(rotation[0]).multiply(
                    new THREE.Matrix4().makeRotationY(rotation[1]).multiply(
                    new THREE.Matrix4().makeRotationZ(rotation[2])))); 
  
  //geo.applyMatrix ( new THREE.Matrix4().makeRotationFromQuaternion(tmpQ) );
  
  return(geo);
};

Composition3D.geometries.cylinderWrapper = function( initP, height, orientation, initR, endR, rSegs, hSegs, open, thetas, thetae ){
  
  // Init - end points (as vectors)
  var start = new THREE.Vector3(initP[0], initP[1], initP[2]);
  
  // Normalize orientation
  var vorientation = new THREE.Vector3(orientation[0], orientation[1], orientation[2]).normalize();
  
  // Compute position (Cylinder is built around its baricenter, initially at 0,0,0 )
  var pos = start.clone().add(vorientation.clone().multiplyScalar(height/2));
  
  // By default the original orientation is 0,1,0
  var origVec = new THREE.Vector3(0,1,0);

  
  // Create default cylinder geo
  var geo = new THREE.CylinderGeometry(initR, endR, height, rSegs, hSegs, open, thetas, thetae );
  
  // Rotation axis is the crossprod of orientation and orig vectors
  var axis = new THREE.Vector3().crossVectors(origVec,vorientation).normalize();
  
  // Rotation angle is arcCos( dotprod ( orig, target))
  var angle = Math.acos(origVec.dot(vorientation));
  
  // Rotate (to align start-end)
  geo.applyMatrix ( new THREE.Matrix4().makeRotationAxis(axis,angle) );

  // NOTE: geometry.translate exists from three.js r72 ... Move to position from origin
  geo.applyMatrix ( new THREE.Matrix4().makeTranslation(pos.x, pos.y, pos.z) );
  
  return(geo);
};

Composition3D.geometries.parametricWrapper = function( fun, translation, rotation, scale, slices, staks ){
  
  // Create parametric base geometry
  var geo = new THREE.ParametricGeometry(fun, slices,staks );
  
  // Translate
  if( translation.some( function(e){ return(e !== 0); } ) )
    geo.applyMatrix ( new THREE.Matrix4().makeTranslation(translation[0],translation[1],translation[2]) );
  
  // Rotate
  if( rotation.some( function(e){ return(e !== 0); } ) ){
    var tmpQ  = new THREE.Quaternion( rotation[0], rotation[1], rotation[2], 1 ).normalize();
    geo.applyMatrix ( new THREE.Matrix4().makeRotationX(rotation[0]).multiply(
                      new THREE.Matrix4().makeRotationY(rotation[1]).multiply(
                      new THREE.Matrix4().makeRotationZ(rotation[2])))); 
  }
                    
  // Scale
  if( scale.some( function(e){ return(e !== 1); } ) )
    geo.applyMatrix( new THREE.Matrix4().makeScale(scale[0], scale[1], scale[2]) );
    
  return(geo);
                    
};

Composition3D.geometries.torusWrapper = function( radius, tube, rsegs, lsegs, angle, center, rotation){
  
  // Create Torus base geometry
  var geo = new THREE.TorusGeometry(radius, tube, rsegs, lsegs, angle);
  
  // Translate
  if( center.some( function(e){ return(e !== 0); } ) )
    geo.applyMatrix ( new THREE.Matrix4().makeTranslation(center[0],center[1],center[2]) );
  
  // Rotate
  if( rotation.some( function(e){ return(e !== 0); } ) ){
    var tmpQ  = new THREE.Quaternion( rotation[0], rotation[1], rotation[2], 1 ).normalize();
    geo.applyMatrix ( new THREE.Matrix4().makeRotationX(rotation[0]).multiply(
                      new THREE.Matrix4().makeRotationY(rotation[1]).multiply(
                      new THREE.Matrix4().makeRotationZ(rotation[2])))); 
  }
    
  return(geo);
};