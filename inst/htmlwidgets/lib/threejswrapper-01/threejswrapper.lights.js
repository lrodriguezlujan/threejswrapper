Composition3D.lights = {};

Composition3D.lights.ambientWrapper = function( color ){
  // Just create ambient light  
  return( new THREE.AmbientLight( new THREE.Color(color) ) );

};


Composition3D.lights.directionalWrapper = function( color, intensity , pos ){
  
  // Create directional light
  var l =  new THREE.DirectionalLight( new THREE.Color(color), intensity ) ;
  
  // Move to position
  l.position.set( pos[0], pos[1], pos[2] );
  
  return(l);
};

Composition3D.lights.hemisphereWrapper = function( color, ground, intensity ){
  
  // Create light
  return( new THREE.hemisphereLight( new THREE.Color(color), new THREE.Color(ground), intensity ) );

};