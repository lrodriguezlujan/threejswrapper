 // Helpers
Composition3D.helpers = {};

Composition3D.helpers.bboxWrapper = function( color, diagonals, obj ){
  
  var bbox;
  
  //create bbox  
  if(diagonals){
    bbox  = new THREE.BoundingBoxHelper(obj, new THREE.Color(color) );
    bbox.update();
  }
  else{
    bbox  = new THREE.BoxHelper(obj);
    bbox.material.color =  new THREE.Color(color);
    bbox.update(obj);
  }
  
  return(bbox);
};


Composition3D.helpers.axisWrapper = function( size, center ){
  
  // Create axis
  var axis = new THREE.AxisHelper( size );
  
  // Move
  axis.applyMatrix ( new THREE.Matrix4().makeTranslation(center[0],center[1],center[2]) );
  
  return(axis);
  
};

Composition3D.helpers.gridWrapper = function(size, step, gridcolor, centercolor, center, rotation ){
  
  var grid = new THREE.GridHelper(size,step);
  
  // Set grid color
  grid.setColors( new THREE.Color(centercolor), new THREE.Color(gridcolor) );
  
  // Change center position
  grid.applyMatrix ( new THREE.Matrix4().makeTranslation(center[0],center[1],center[2]) );
  
  // Apply rotations
  grid.applyMatrix ( new THREE.Matrix4().makeRotationX(rotation[0]).multiply(
                    new THREE.Matrix4().makeRotationY(rotation[1]).multiply(
                    new THREE.Matrix4().makeRotationZ(rotation[2])))); 
  
  return (grid);
};