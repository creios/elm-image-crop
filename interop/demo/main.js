window.addEventListener('load', function () {
    var image = document.getElementById('image');
    var node = document.getElementById('main');
    var app = Elm.ImageCrop.Interop.embed(node, {
        image: {width: 1800, height: 1200},
        cropAreaWidth: getImageWidth(),
        offset: {x: 20, y: 20},
        selection: {topLeft: {x: 0, y: 0}, bottomRight: {x: 100, y: 100}},
        aspectRatio: null
    });
    var
        coordinates = document.getElementById('coordinates'),
        noSelection = document.getElementById('no-selection'),
        topLeftX = document.getElementById('top-left-x'),
        topLeftY = document.getElementById('top-left-y'),
        bottomRightX = document.getElementById('bottom-right-x'),
        bottomRightY = document.getElementById('bottom-right-y');
    app.ports.selectionChanged.subscribe(function (selection) {
        if (selection !== null) {
            topLeftX.innerHTML = selection.topLeft.x;
            topLeftY.innerHTML = selection.topLeft.y;
            bottomRightX.innerHTML = selection.bottomRight.x;
            bottomRightY.innerHTML = selection.bottomRight.y;
            coordinates.style.display = 'block';
            noSelection.style.display = 'none';
        } else {
            coordinates.style.display = 'none';
            noSelection.style.display = 'block';
        }
    });
    window.addEventListener('resize', function () {
        app.ports.viewportChanged.send(getImageWidth());
    });
    function getImageWidth() {
        return +getComputedStyle(image).width.replace('px', '');
    }
});
