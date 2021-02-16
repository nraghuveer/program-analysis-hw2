
var i = 0;
var j = 10; 
while (i < 4) {
	console.log(i);
	i += 1;

	while (j > 4) {
		console.log(j); 
		if ((j % 2) !== 0) { 
			console.log(j + ' is odd.');
		}
	}
	console.log('i = ' + i);
	console.log('j = ' + j);
};