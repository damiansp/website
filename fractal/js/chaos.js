var chaos = (function() {
    return {
	/**
	 * Initialize chaos by finding the canvas on the page and resizing it 
	 * to the full size of the browser
	 */
	init: function() {
	    this.canvas = document.getElementById('canvas');
	    this.context = this.canvas.getContext('2d');
	    this.setSize(window.innerWidth, window.innerHeight);
	},
	setSize: function(width, height) {
	    this.width = this.canvas.width = width;
	    this.height = this.canvas.height = height;
	},
	
	/**
	 * Clears the canvas by filling it with the color specified, or erasing
	 * all pixels if no color is specified.
	 */
	clear: function(color) {
	    if(color) {
		this.context.fillStyle = color;
		this.context.fillRect(0, 0, this.width, this.height);
	    } else {
		this.context.clearRect(0, 0, this.width, this.height);
	    }
	},

	/**
	 * Pops up a bitmap image representation of the canvas in a new window.
	 */
	popImage: function() {
	    var win = window.open('', 'Canvas Image'),
	    src = this.canvas.toDataURL('image/png');

	    win.document.write('<img src="' + src + '" width="' + this.width +
			       '" height="' + this.height + '"/>');
	}
    };
}());