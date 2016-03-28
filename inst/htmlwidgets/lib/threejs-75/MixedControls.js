/**
 * Fly controls:
 * @author James Baicoianu / http://www.baicoianu.com/
 * 
 * Orbit:
 * @author qiao / https://github.com/qiao
 * @author mrdoob / http://mrdoob.com
 * @author alteredq / http://alteredqualia.com/
 * @author WestLangley / http://github.com/WestLangley
 * @author erich666 / http://erichaines.com
 * 
 * Gamepad and unification+ fly modifications:
 * @author Luis Rodríguez Lujan / http://cig.fi.upm.es/?q=luis_rodriguez
 * 
 * @seealso
 * https://developer.mozilla.org/en-US/docs/Web/API/Gamepad_API/Using_the_Gamepad_API
 * https://w3c.github.io/gamepad/#remapping
 * http://luser.github.io/gamepadtest/
 * https://hacks.mozilla.org/2013/12/the-gamepad-api/
 */
  
  THREE.MixedControls = function ( object , domElement, args  ) {
    
    /* Default values */
    
    // Base elements
    this.object = object;
    this.domElement = ( domElement !== undefined ) ? domElement : document;
    if ( domElement ) this.domElement.setAttribute( 'tabindex', -1 );
    
    // Disables all controls
    this.enabled = true;
    
    // Enable/disable controls by input
    this.mouseEnabled = true;
    this.keyEnabled = true;
    this.padEnabled = true;
    this.touchEnabled = true;
    
    // Enable/diable controls by function
    this.rotationEnabled  = true;
    this.panEnabled  = true;
    this.dollyEnabled  = true;
    this.movementEnabled  = true;
    
    // API
    
    // Speed in "units" per second ( units moved if a key is pressed 1 sec )
    this.movementSpeed = 10.0;
    this.rollSpeed = 0.25;
    this.updateThreshold = 1E-5;
    
    // Mouse speeds
    this.rotateSpeed = 1.0;
    this.zoomSpeed = 1.0;
    
    /**
     *  FROM ORBIT
     */
     
   	// "target" sets the location of focus, where the control orbits around
	  // and where it pans with respect to.
	  this.target = new THREE.Vector3();
     
  	// Limits to how far you can dolly in and out
  	this.minDistance = 0;
  	this.maxDistance = Infinity;
  
  	// How far you can orbit vertically, upper and lower limits.
  	// Range is 0 to Math.PI radians.
  	this.minPolarAngle = 0; // radians
  	this.maxPolarAngle = Math.PI; // radians
  
  	// How far you can orbit horizontally, upper and lower limits.
  	// If set, must be a sub-interval of the interval [ - Math.PI, Math.PI ].
  	this.minAzimuthAngle = - Infinity; // radians
  	this.maxAzimuthAngle = Infinity; // radians
    
    /**
     *  CONTROL MAPPING
     */
    // Mouse buttons
	  this.mouseButtons = { ORBIT: THREE.MOUSE.LEFT, ZOOM: THREE.MOUSE.MIDDLE, PAN: THREE.MOUSE.RIGHT };
	  
	  // FWD: W, BACK: S, LEFT: A, RIGHT: D, UP: R, DOWN: F, PITCHUP : arrow_up, PITCHDOWN : arrow_down, YAWLEFT: arrow_left, YAWRIGHT: arrow_right, ROLLLEFT: Q, ROLLRIGHT: E
	  this.keyButtons = { FWD: 87, BACK: 83, 
	                      LEFT: 65, RIGHT: 68, UP: 82, DOWN: 70,  
	                      PITCHUP: 38, PITCHDOWN: 40,
	                      YAWLEFT: 37, YAWRIGHT: 39,
	                      ROLLLEFT: 81, ROLLRIGHT: 69 };
	                      
    // MODAL
    this.withModal = true;
	                      
	  // for reset
    
    this.target0 = this.target.clone();
    this.position0 = this.object.position.clone();
    this.up0 = this.object.position.clone();
	                      
	  /* Extend (get args) Mimics Jquery extend -http://stackoverflow.com/questions/11197247/javascript-equivalent-of-jquerys-extend-method */
	  function extend(){
      for(var i=1; i<arguments.length; i++)
        for(var key in arguments[i])
            if(arguments[i].hasOwnProperty(key))
                arguments[0][key] = arguments[i][key];
      return arguments[0];
    }
	  extend(this, args );
	   
    /**
      * INTERNALS
    */
    var scope = this; // Cache this
    
    // Previous position / quater since last update
    var prevPosition = new THREE.Vector3(); 
    var prevQuaternion = new THREE.Quaternion();
    var prevUp = new THREE.Vector3();
    
    // Change event ( to trigger rendering )
    var changeEvent = { type: 'change' };
    var startEvent = { type: 'start'};
	  var endEvent = { type: 'end'};
    
    // internals
    var tmpQuaternion = new THREE.Quaternion();
    var moveState = { up: 0, down: 0, left: 0, right: 0, forward: 0, back: 0, pitchUp: 0, pitchDown: 0, yawLeft: 0, yawRight: 0, rollLeft: 0, rollRight: 0 };
    var moveVector = new THREE.Vector3( 0, 0, 0 );
    var rotationVector = new THREE.Vector3( 0, 0, 0 );
    
    /** FROM ORBIT CONTROLS **/
    
      // Rotation
    	var rotateStart = new THREE.Vector2();
    	var rotateEnd = new THREE.Vector2();
    	var rotateDelta = new THREE.Vector2();
    
      // Pan
    	var panStart = new THREE.Vector2();
    	var panEnd = new THREE.Vector2();
    	var panDelta = new THREE.Vector2();
    	var panOffset = new THREE.Vector3();
    
      // ?
    	var offset = new THREE.Vector3();
    
      // Dolly
    	var dollyStart = new THREE.Vector2();
    	var dollyEnd = new THREE.Vector2();
    	var dollyDelta = new THREE.Vector2();
    
    	var theta;
    	var phi;
    	var psi; // Rotation
    	var phiDelta = 0;
    	var thetaDelta = 0;
    	var psiDelta = 0;
    	var scale = 1;
    	var pan = new THREE.Vector3();
    
    	var STATE = { NONE : -1, ROTATE : 0, DOLLY : 1, PAN : 2, TOUCH_ROTATE : 3, TOUCH_DOLLY : 4, TOUCH_PAN : 5 };
    
    	var state = STATE.NONE;
    

    
    	// so camera.up is the orbit axis
    
    	var quat = new THREE.Quaternion().setFromUnitVectors( object.up, new THREE.Vector3( 0, 1, 0 ) );
    	var quatInverse = quat.clone().inverse();
    	
    /*******************
     * 
     *      METHODS 
     * 
     * *****************/
    this.handleEvent = function ( event ) {
      if ( typeof this[ event.type ] == 'function' ) {
        this[ event.type ]( event );
      }
    };
    
    /** PUBLIC METHODS **/
    
    // ROTATION
  	this.rotateLeft = function ( angle ) {
  	  // NO auto rotation
  		if (typeof angle == "undefined")	return ;
  		
  		thetaDelta -= angle ; //
  	};
  
  	this.rotateUp = function ( angle ) {
  	  // NO auto rotation
  		if (typeof angle == "undefined")	return ;
  		
  		phiDelta -= angle ;
  	};
  
    // PAN
    
  	// pass in distance in world space to move left
  	this.panLeft = function ( distance ) {
  
  		var te = this.object.matrix.elements;
  
  		// get X column of matrix
  		panOffset.set( te[ 0 ], te[ 1 ], te[ 2 ] );
  		panOffset.multiplyScalar( - distance );
  
  		pan.add( panOffset );
  
  	};
  
  	// pass in distance in world space to move up
  	this.panUp = function ( distance ) {
  
  		var te = this.object.matrix.elements;
  
  		// get Y column of matrix
  		panOffset.set( te[ 4 ], te[ 5 ], te[ 6 ] );
  		panOffset.multiplyScalar( distance );
  
  		pan.add( panOffset );
  
  	};
  
  	// pass in x,y of change desired in pixel space,
  	// right and down are positive
  	this.pan = function ( deltaX, deltaY ) {
  
  		var element = scope.domElement === document ? scope.domElement.body : scope.domElement;
  
      // Perspective camera
  		if ( scope.object.fov !== undefined ) {
  
  			// perspective
  			var position = scope.object.position;
  			var offset = position.clone().sub( scope.target );
  			var targetDistance = offset.length();
  
  			// half of the fov is center to top of screen
  			targetDistance *= Math.tan( ( scope.object.fov / 2 ) * Math.PI / 180.0 );
  
  			// we actually don't use screenWidth, since perspective camera is fixed to screen height
  			scope.panLeft( 2 * deltaX * targetDistance / element.clientHeight );
  			scope.panUp( 2 * deltaY * targetDistance / element.clientHeight );
  
  		} // Orthographic camera
  		else if ( scope.object.top !== undefined ) {
  
  			// orthographic
  			scope.panLeft( deltaX * (scope.object.right - scope.object.left) / element.clientWidth );
  			scope.panUp( deltaY * (scope.object.top - scope.object.bottom) / element.clientHeight );
  
  		} else {
  
  			// camera neither orthographic or perspective
  			console.warn( 'WARNING: unknown camera type - pan disabled.' );
  
  		}
  
  	};
  
    // DOLLY
  	this.dollyIn = function ( dollyScale ) {
  
  		if ( typeof dollyscale == "undefined" ) dollyScale = getZoomScale();
  		scale /= dollyScale;
  
  	};
  
  	this.dollyOut = function ( dollyScale ) {
  		if ( typeof dollyscale == "undefined" ) dollyScale = getZoomScale();
  		scale *= dollyScale;
  	};
  	
  	// TODO
  	
  	
  	
  	// AUXILIAR
  	
  	this.reset = function () {

		  state = STATE.NONE;

      // Reset target
	  	this.target.copy( this.target0 );
		
		  // Reset object position
		  this.object.position.copy( this.position0 );
		  
		  // Update up direction
		  this.object.up.copy( this.up0 );

      // Lot has changed
		  this.update();

	  };

  	this.getPolarAngle = function () {
  
  		return phi;
  
  	};
  
  	this.getAzimuthalAngle = function () {
  
  		return theta;
  
  	};
  	
  	/** CREATE HELP MODAL **/
  	
  	// Only for shiny
  	this.createHelperModal = function() {
  	  
  	  if( this.withModal === false ) return;
  	  
  	  // Add link that triggers modal
  	  var trigLink = document.createElement("a");
  	  trigLink.setAttribute("data-toggle","modal");
  	  trigLink.setAttribute("href","#mixedControlsModal");
  	  trigLink.innerHTML = 'Controls help';
  	  
  	  // Set Style
  	  trigLink.style.color = "blue";
  	  trigLink.style.position = "absolute";
  	  trigLink.style.top = "0px";
  	  trigLink.style.left = "25px";
  	  trigLink.style.padding = "5px";
  	  trigLink.style.zIndex = 100;
  	  trigLink.style.fontWeight = "bold";
  	  
  	  scope.domElement.appendChild(trigLink);
  	  
  	  // Add modal
  	  var modal = document.createElement("div");
  	  modal.setAttribute("id","mixedControlsModal");
  	  modal.setAttribute("class", "modal fade");
  	  modal.setAttribute("role", "dialog");
  	  
  	  // Add modal content
  	  modal.innerHTML =' \
  	  <div class="modal-dialog">\
        <div class="modal-content">\
          <div class="modal-header">\
            <button type="button" class="close" data-dismiss="modal">&times;</button>\
            <h4 class="modal-title">Controls</h4>\
          </div>\
          <div class="modal-body">\
            <h2> Mouse controls </h2>\
            <ul>\
            <li> <b> Left button </b> Rotate  </li> \
            <li> <b> Right button </b> Pan </li> \
            <li> <b> Wheel </b> Zoom </li> \
            </ul> \
            <h2> Keyboard controls </h2>\
            <ul>\
            <li> <b> WASD </b> Move  </li> \
            <li> <b> R,F </b> Up, Down </li> \
            <li> <b> Q,E </b> Roll </li> \
            <li> <b> up,down arrows </b> Pitch </li>  \
            <li> <b> left,right arrows </b> Yaw </li> \
            </ul> \
            <h2> Touchpad controls </h2>\
            <ul>\
            <li> <b> One finger </b> Rotate  </li> \
            <li> <b> Two finger </b> Zoom </li> \
            <li> <b> Three finger </b> Pan </li> \
            </ul> \
          </div>\
          <div class="modal-footer">\
            <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>\
          </div>\
        </div>\
      </div>';
      scope.domElement.appendChild(modal);
  	};
    
    /** PRIVATE METHODS **/
    
    // Zoom scale (0.95^zoom_spped to provide smooth zooming)
   	function getZoomScale() {
        return Math.pow( 0.95, scope.zoomSpeed );
	  }
	  
	  /*** MOUSE LISTENERS ****/
	  
	  // Start tracking when movement starts
	  function onMouseDown( event ) {

  		if ( ( scope.enabled === false ) || ( scope.mouseEnabled === false ) ) return;
  		event.preventDefault();
      
      // ORBIT action
  		if ( event.button === scope.mouseButtons.ORBIT ) {
  		  
  		  // Start rotation (Get mouse position)
  			if ( scope.rotationEnabled === false ) return;
  			state = STATE.ROTATE;
  			rotateStart.set( event.clientX, event.clientY );
  
  		} else if ( event.button === scope.mouseButtons.ZOOM ) {
  		  
  		  // Start dolly (get mouse position)
  			if ( scope.dollyEnabled === false ) return;
  			state = STATE.DOLLY;
  			dollyStart.set( event.clientX, event.clientY );
  
  		} else if ( event.button === scope.mouseButtons.PAN ) {
  		  
  		  // Start pan (get mouse position)
  			if ( scope.panEnabled === false ) return;
  			state = STATE.PAN;
  			panStart.set( event.clientX, event.clientY );
  
  		}
  
      // If we are in the middle of an action - set listeners
  		if ( state !== STATE.NONE ) {
  			document.addEventListener( 'mousemove', onMouseMove, false );
  			document.addEventListener( 'mouseup', onMouseUp, false );
  			// Event: we are listening
  			scope.dispatchEvent( startEvent );
  		}
	}
	
	  // Track mouse movement
		function onMouseMove( event ) {

  	  if ( ( scope.enabled === false ) || ( scope.mouseEnabled === false ) ) return;
  
  		event.preventDefault();
  
  		var element = scope.domElement === document ? scope.domElement.body : scope.domElement;
  
      /// ROTATION
  		if ( state === STATE.ROTATE ) {
  
  			if ( scope.rotationEnabled === false ) return;
        // Update rotation end an delta
  			rotateEnd.set( event.clientX, event.clientY );
  			rotateDelta.subVectors( rotateEnd, rotateStart );
  
  			// rotating across whole screen goes 360 degrees around
  			scope.rotateLeft( 2 * Math.PI * rotateDelta.x / element.clientWidth * scope.rotateSpeed );
  
  			// rotating up and down along whole screen attempts to go 360, but limited to 180
  			scope.rotateUp( 2 * Math.PI * rotateDelta.y / element.clientHeight * scope.rotateSpeed );
  			
        // Rotation done
  			rotateStart.copy( rotateEnd );
  		} 
  		// DOLLY
  		else if ( state === STATE.DOLLY ) {
  
  			if ( scope.dollyEnabled === false ) return;
  			
        // Update dolly end and delta
  			dollyEnd.set( event.clientX, event.clientY );
  			dollyDelta.subVectors( dollyEnd, dollyStart );
  
        // Perform movements
  			if ( dollyDelta.y > 0 ) {
  				scope.dollyIn();
  
  			} else {
  				scope.dollyOut();
  			}
  			// Movement done
  			dollyStart.copy( dollyEnd );
  		}
  		/// PAN
  		else if ( state === STATE.PAN ) {
  
  			if ( scope.panEnabled === false ) return;
        // Track movement
  			panEnd.set( event.clientX, event.clientY );
  			panDelta.subVectors( panEnd, panStart );
        // Pan
  			scope.pan( panDelta.x, panDelta.y );
  			
        // Change start
  			panStart.copy( panEnd );
  
  		}
      // ¿? Why this is needed? Its suppose to never happen ... ?
  		if ( state !== STATE.NONE ) scope.update();
  	}

    // Stop listening mouse 
	  function onMouseUp() {

		  if ( ( scope.enabled === false ) || ( scope.mouseEnabled === false ) ) return;

  		document.removeEventListener( 'mousemove', onMouseMove, false );
  		document.removeEventListener( 'mouseup', onMouseUp, false );
  		
  		// We are not listening anymore
  		scope.dispatchEvent( endEvent );
  		
  		// We are not moving (at least by mouse)
  		state = STATE.NONE;
  	}
  	
  	// Wheel zoom
  	function onMouseWheel( event ) {

      // Lot of conditions
  		if ( scope.enabled === false || scope.mouseEnabled === false || scope.dollyEnabled === false || state !== STATE.NONE ) return;

  		event.preventDefault();
  		event.stopPropagation();
  
  		var delta = 0;
  
  		if ( typeof event.wheelDelta !== "undefined" ) { // WebKit / Opera / Explorer 9
  			delta = event.wheelDelta;
  		} else if ( typeof event.detail !== "undefined" ) { // Firefox
  			delta = - event.detail;
  		}
  
  		if ( delta > 0 ) {
  			scope.dollyOut();
  		} else {
  			scope.dollyIn();
  		}
  
      // Do we really need to call update?!
  		scope.update();
  		
  		scope.dispatchEvent( startEvent );
  		scope.dispatchEvent( endEvent );
	  }
  
    /** TOUCHSCREEN LISTENERS ***/
    
    function touchstart( event ) {

		if ( scope.enabled === false || scope.touchEnabled === false ) return;

		switch ( event.touches.length ) {

			case 1:	// one-fingered touch: rotate

				if ( scope.rotationEnabled === false ) return;
				state = STATE.TOUCH_ROTATE;
				rotateStart.set( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY );
				
				break;

			case 2:	// two-fingered touch: dolly

				if ( scope.zoomEnabled === false ) return;

				state = STATE.TOUCH_DOLLY;
				var dx = event.touches[ 0 ].pageX - event.touches[ 1 ].pageX;
				var dy = event.touches[ 0 ].pageY - event.touches[ 1 ].pageY;
				var distance = Math.sqrt( dx * dx + dy * dy );
				dollyStart.set( 0, distance );
				
				break;

			case 3: // three-fingered touch: pan

				if ( scope.panEnabled === false ) return;

				state = STATE.TOUCH_PAN;
				panStart.set( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY );
				
				break;

			default:
				state = STATE.NONE;
		}
    
    // Start tracking
		if ( state !== STATE.NONE ) scope.dispatchEvent( startEvent );

	}
	
  	function touchmove( event ) {
  
  		if ( scope.enabled === false || scope.touchEnabled === false ) return;
  
  		event.preventDefault();
  		event.stopPropagation();
  
  		var element = scope.domElement === document ? scope.domElement.body : scope.domElement;
  
  		switch ( event.touches.length ) {
  
  			case 1: // one-fingered touch: rotate
  
  				if ( scope.rotationEnabled === false ) return;
  				if ( state !== STATE.TOUCH_ROTATE ) return;
  
  				rotateEnd.set( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY );
  				rotateDelta.subVectors( rotateEnd, rotateStart );
  
  				// rotating across whole screen goes 360 degrees around
  				scope.rotateLeft( 2 * Math.PI * rotateDelta.x / element.clientWidth * scope.rotateSpeed );
  				// rotating up and down along whole screen attempts to go 360, but limited to 180
  				scope.rotateUp( 2 * Math.PI * rotateDelta.y / element.clientHeight * scope.rotateSpeed );
  
  				rotateStart.copy( rotateEnd );
  
  				scope.update();
  				
  				break;
  
  			case 2: // two-fingered touch: dolly
  
  				if ( scope.dollyEnabled === false ) return;
  				if ( state !== STATE.TOUCH_DOLLY ) return;
  
  				var dx = event.touches[ 0 ].pageX - event.touches[ 1 ].pageX;
  				var dy = event.touches[ 0 ].pageY - event.touches[ 1 ].pageY;
  				var distance = Math.sqrt( dx * dx + dy * dy );
  
  				dollyEnd.set( 0, distance );
  				dollyDelta.subVectors( dollyEnd, dollyStart );
  
  				if ( dollyDelta.y > 0 ) {
  					scope.dollyOut();
  				} else {
  					scope.dollyIn();
  				}
  
  				dollyStart.copy( dollyEnd );
  
  				scope.update();
  				
  				break;
  
  			case 3: // three-fingered touch: pan
  
  				if ( scope.panEnabled === false ) return;
  				if ( state !== STATE.TOUCH_PAN ) return;
  
  				panEnd.set( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY );
  				panDelta.subVectors( panEnd, panStart );
  
  				scope.pan( panDelta.x, panDelta.y );
  
  				panStart.copy( panEnd );
  
  				scope.update();
  				break;
  
  			default:
  
  				state = STATE.NONE;
  
  		}
  
  	}
  	
  	function touchend() {
		
		  if ( ( scope.enabled === false )  || ( scope.touchEnabled === false ) ) return;

		  scope.dispatchEvent( endEvent );
		  state = STATE.NONE;
	  }
    
    /** GAMEPAD LISTENERS ***/
  	
    // Gamepad axis controls listener
    /* function gpad_axis (event ){
      
      var value = event.value;
      var inv = false;
      
      inv = ( value < 0);
      value = Math.abs(value);
      if ( value < 0.1 ) value = 0;
      
      switch(event.axis){
        case "LEFT_STICK_X": if(inv){  moveState.left=value; moveState.right=0;} else { moveState.right=value; moveState.left = 0} break; 
        case "LEFT_STICK_Y": if(!inv){ moveState.back=value; moveState.forward=0; }else{ moveState.forward=value; moveState.back=0; } break;
        case "RIGHT_STICK_X": if(inv){ moveState.yawLeft=value; moveState.yawRight=0; }else{ moveState.yawRight=value; moveState.yawLeft=0; } break;
        case "RIGHT_STICK_Y": if(!inv){ moveState.pitchDown=value; moveState.pitchUp=0; }else{ moveState.pitchUp=value; moveState.pitchDown=0; } break;
      }
      
      updateMovementVector();
      updateRotationVector();
      
    } */
    
    // Gamepad button up control listener
    /* function gpad_bup(event){
      switch(event.control){
        
        case "DPAD_UP":  moveState.up = 0; break;
        case "DPAD_DOWN":  moveState.down = 0; break;
        
        case "LEFT_TOP_SHOULDER":  moveState.rollLeft = 0; break;
        case "RIGHT_TOP_SHOULDER":  moveState.rollRight = 0; break;
      }
      updateMovementVector();
      updateRotationVector();
    }*/
    
    
    // Gamepad button down control listener
    /*
    function gpad_bdown( event ){
      
      switch(event.control){
        
        case "DPAD_UP":  moveState.up = 1; break;
        case "DPAD_DOWN":  moveState.down = 1; break;
        
        case "LEFT_TOP_SHOULDER": moveState.rollLeft = 1; break;
        case "RIGHT_TOP_SHOULDER": moveState.rollRight = 1; break;
      }
      this.updateMovementVector();
      this.updateRotationVector();
    }*/
    
    /** KB LISTENERS **/
    
    // Keydown listener
    function keydown( event ) {
      
      if ( ( scope.enabled === false ) || ( scope.keyEnabled === false ) ) return;
      
      // Do not listener to ALT + CTRL + stuff
      if ( event.altKey ) {
        return;
      }
      
      //event.preventDefault();
  		//event.stopPropagation();
      
      switch ( event.keyCode ) {
        
        case scope.keyButtons.FWD : moveState.forward = 1; break;
        case scope.keyButtons.BACK:  moveState.back = 1; break;
        
        case scope.keyButtons.LEFT: moveState.left = 1; break;
        case scope.keyButtons.RIGHT: moveState.right = 1; break;
        
        case scope.keyButtons.UP: moveState.up = 1; break;
        case scope.keyButtons.DOWN: moveState.down = 1; break;
        
        case scope.keyButtons.PITCHUP: moveState.pitchUp = 1; break;
        case scope.keyButtons.PITCHDOWN: moveState.pitchDown = 1; break;
        
        case scope.keyButtons.YAWLEFT: moveState.yawLeft = 1; break;
        case scope.keyButtons.YAWRIGHT: moveState.yawRight = 1; break;
        
        case scope.keyButtons.ROLLLEFT: moveState.rollLeft = 1; break;
        case scope.keyButtons.ROLLRIGHT: moveState.rollRight = 1; break;

      }
      
      if(scope.movementEnabled) updateMovementVector();
      if(scope.rotationEnabled) updateRotationVector();
    }
    
    // KeyUp listener
    function keyup( event ) {
      
      if ( ( scope.enabled === false ) || ( scope.keyEnabled === false ) ) return;
      
      //event.preventDefault();
  		//event.stopPropagation();
      
      switch ( event.keyCode ) {
        
        case scope.keyButtons.FWD : moveState.forward = 0; break;
        case scope.keyButtons.BACK:  moveState.back = 0; break;
        
        case scope.keyButtons.LEFT: moveState.left = 0; break;
        case scope.keyButtons.RIGHT: moveState.right = 0; break;
        
        case scope.keyButtons.UP: moveState.up = 0; break;
        case scope.keyButtons.DOWN: moveState.down = 0; break;
        
        case scope.keyButtons.PITCHUP: moveState.pitchUp = 0; break;
        case scope.keyButtons.PITCHDOWN: moveState.pitchDown = 0; break;
        
        case scope.keyButtons.YAWLEFT: moveState.yawLeft = 0; break;
        case scope.keyButtons.YAWRIGHT: moveState.yawRight = 0; break;
        
        case scope.keyButtons.ROLLLEFT: moveState.rollLeft = 0; break;
        case scope.keyButtons.ROLLRIGHT: moveState.rollRight = 0; break;
        
      }
      
      if(scope.movementEnabled) updateMovementVector();
      if(scope.rotationEnabled) updateRotationVector();
    }
    
    /** GPAD AND KEYBOARD UPDATES */
    
    // Update move vector
    function updateMovementVector() {
      
      moveVector.x = ( moveState.left    - moveState.right );
      moveVector.y = ( -moveState.down    + moveState.up );
      moveVector.z = ( -moveState.forward  + moveState.back );
    }
    
    // Update rotation
    function updateRotationVector() {
      
      rotationVector.x = ( -moveState.pitchDown + moveState.pitchUp );
      rotationVector.y = ( -moveState.yawRight  + moveState.yawLeft );
      rotationVector.z = ( -moveState.rollRight + moveState.rollLeft );
      
    }
    
    
    /** AUXILIAR **/
    function getContainerDimensions() {
      
      if ( this.domElement != document ) {
        
        return {
          size	: [ this.domElement.offsetWidth, this.domElement.offsetHeight ],
          offset	: [ this.domElement.offsetLeft,  this.domElement.offsetTop ]
        };
        
      } else {
        
        return {
          size	: [ window.innerWidth, window.innerHeight ],
          offset	: [ 0, 0 ]
        };
        
      }
      
    }
    
    function bind( scope, fn ) {
      
      return function () {
        
        fn.apply( scope, arguments );
        
      };
      
    }
    
    /** ¡¡¡UPDATE METHOD!!!! **/
    this.update = function( delta ) {
      
      var position = this.object.position;
      
      // Compute camera - target distance
      var currentDist = this.object.position.clone().sub(this.target).length();
      
      if( typeof delta == "undefined" ) delta = 0;
      
      // Movement updates (FROM FLY CONTROLS)
      if(delta > 0){
        
        var moveMult = delta * scope.movementSpeed;
        var rotMult = delta * scope.rollSpeed;
      
        // Movement (PAN)
        scope.panLeft( moveVector.x * moveMult )
        scope.panUp(  moveVector.y * moveMult )
        if(moveVector.z > 0 )
          this.dollyIn(  0.95 );
        else if(moveVector.z < 0)
          this.dollyOut(  0.95 );
        //scope.moveFwd( moveVector.z * moveMult )
        
        // Rotate camera
        psiDelta = rotationVector.z * rotMult;
        
        tmpQuaternion.set( rotationVector.x * rotMult, rotationVector.y * rotMult, 0, 1 ).normalize();
        this.object.quaternion.multiply( tmpQuaternion );
      
        // expose the rotation vector for convenience
        this.object.rotation.setFromQuaternion( this.object.quaternion, this.object.rotation.order );
      
        // Get current camera "view" vector and set new target position
        var targetVector = new THREE.Vector3(0,0,-1)
        targetVector.applyQuaternion(this.object.quaternion).normalize().multiplyScalar(currentDist)
        targetVector.add(this.object.position)
        
        this.target = targetVector;
      }
      
       // update : mouse and touch
      
       // Vector between target and position ( target -> position )
    		offset.copy( position ).sub( this.target );
    
    		// rotate offset to "y-axis-is-up" space
    		offset.applyQuaternion( quat );
    
    		// angle from z-axis around y-axis
    		theta = Math.atan2( offset.x, offset.z );
    
    		// angle from y-axis
    		phi = Math.atan2( Math.sqrt( offset.x * offset.x + offset.z * offset.z ), offset.y );
        
        // Rotation (next movement)
    		theta += thetaDelta;
    		phi += phiDelta;
    
    		// restrict theta to be between desired limits
    		theta = Math.max( this.minAzimuthAngle, Math.min( this.maxAzimuthAngle, theta ) );
    
    		// restrict phi to be between desired limits
    		phi = Math.max( this.minPolarAngle, Math.min( this.maxPolarAngle, phi ) );
    
    		// restrict phi to be between 1E-5 and PI-1E-5
    		phi = Math.max( 1E-5, Math.min( Math.PI - 1E-5, phi ) );
    
        // Distance to target
    		var radius = offset.length() * scale;
    
    		// restrict radius to be between desired limits
    		radius = Math.max( this.minDistance, Math.min( this.maxDistance, radius ) );
    
    		// move target to panned location
    		this.target.add( pan );
    
        // Compute displacement (compute next vector)
    		offset.x = radius * Math.sin( phi ) * Math.sin( theta );
    		offset.y = radius * Math.cos( phi );
    		offset.z = radius * Math.sin( phi ) * Math.cos( theta );
    
    		// rotate offset back to "camera-up-vector-is-up" space
    		offset.applyQuaternion( quatInverse );
    
        // Update position
    		position.copy( this.target ).add( offset );
    		
    		// Rotate camera
    		if(psiDelta !== 0){
          tmpQuaternion.set( 0, 0, psiDelta, 1 ).normalize();
          this.object.up.applyQuaternion( tmpQuaternion );
          
          // update quad and quat inverse
          quat.setFromUnitVectors( this.object.up, new THREE.Vector3( 0, 1, 0 ) );
    	    quatInverse = quat.clone().inverse();
    		}
      
        // expose the rotation vector for convenience
        this.object.rotation.setFromQuaternion( this.object.quaternion, this.object.rotation.order );
    
        // Always look at the target
    		this.object.lookAt( this.target );
    		
    		// Z rotation
    		/* if( zrotation > 0 ){
    		  tmpQuaternion.set( 0, 0, zrotation, 1 ).normalize();
          this.object.quaternion.multiply( tmpQuaternion );
          // expose the rotation vector for convenience
          this.object.rotation.setFromQuaternion( this.object.quaternion, this.object.rotation.order );
    		}*/
    
        // Reset deltas and stuff
    		thetaDelta = 0;
    		phiDelta = 0;
    		psiDelta = 0;
    		scale = 1;
    		pan.set( 0, 0, 0 );
    	
        // Check if the event should be dispatched
        if ( prevPosition.distanceToSquared( this.object.position ) > this.updateThreshold ||
           8 * (1 - prevQuaternion.dot(this.object.quaternion)) > this.updateThreshold  || 
            8 * (1 - prevUp.dot(this.object.up)) > this.updateThreshold ){
        
            // Create event
            this.dispatchEvent( changeEvent );
            
            // Update position since last update
            prevPosition.copy( this.object.position );
            prevQuaternion.copy (this.object.quaternion );
            prevUp.copy( this.object.up);
            
        }
    };
    
    // Prevent context menu 
    this.domElement.addEventListener( 'contextmenu', function ( event ) { event.preventDefault(); }, false );
    
    // Keyboard bindings
    window.addEventListener( 'keydown', bind( this, keydown ), false );
    window.addEventListener( 'keyup',   bind( this, keyup ), false );
    window.addEventListener( 'keydown', bind( this, keydown ), false );
    
    // Mouse bindings
    this.domElement.addEventListener( 'contextmenu', function ( event ) { event.preventDefault(); }, false );
	  this.domElement.addEventListener( 'mousedown', onMouseDown, false );
	  this.domElement.addEventListener( 'mousewheel', onMouseWheel, false );
	  this.domElement.addEventListener( 'DOMMouseScroll', onMouseWheel, false ); // firefox

    // Touchscreen bindings
	  this.domElement.addEventListener( 'touchstart', touchstart, false );
	  this.domElement.addEventListener( 'touchend', touchend, false );
	  this.domElement.addEventListener( 'touchmove', touchmove, false );
	  
	  // Gamepad
	  window.addEventListener("gamepadconnected", function(e) {
      console.log("Gamepad connected at index %d: %s. %d buttons, %d axes.",
      e.gamepad.index, e.gamepad.id,
      e.gamepad.buttons.length, e.gamepad.axes.length);
    });
    
    window.addEventListener("gamepaddisconnected", function(e) {
      console.log("Gamepad disconnected from index %d: %s",
      e.gamepad.index, e.gamepad.id);
    });
    
    // Initial update
    updateMovementVector();
    updateRotationVector();
    this.update();
  };
  
  // EventDispatcher and constructor
  THREE.MixedControls.prototype = Object.create( THREE.EventDispatcher.prototype );
  THREE.MixedControls.prototype.constructor = THREE.MixedControls;
  