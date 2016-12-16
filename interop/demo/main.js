window.addEventListener('load', function () {
    var image = document.getElementById('image');
    var node = document.getElementById('main');
    var app = Elm.ImageCrop.Interop.embed(node, {
        image: {width: 1800, height: 1200},
        cropAreaWidth: getImageWidth(),
        selection: {topLeft: {x: 0, y: 0}, bottomRight: {x: 100, y: 100}},
        aspectRatio: null
    });
    var coordinates = document.getElementById('coordinates'),
        noSelection = document.getElementById('no-selection'),
        topLeftX = document.getElementById('top-left-x'),
        topLeftY = document.getElementById('top-left-y'),
        bottomRightX = document.getElementById('bottom-right-x'),
        bottomRightY = document.getElementById('bottom-right-y'),
        aspectRatioInput = document.getElementById('aspect-ratio-input');
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
        return Math.round(+getComputedStyle(image).width.replace('px', ''));
    }
    app.ports.requestOffset.subscribe(function () {
        var rect = image.getBoundingClientRect();
        app.ports.receiveOffset.send({
            x: Math.round(rect.left + window.pageXOffset),
            y: Math.round(rect.top + window.pageYOffset)
        });
    });
    aspectRatioInput.addEventListener('change', function (event) {
        var
            aspectRatios = {
                'free': null,
                'square': {width: 1, height: 1},
                'din-landscape': {width: Math.sqrt(2), height: 1},
                'din-portrait': {width: 1, height: Math.sqrt(2)}
            },
            aspectRatio = aspectRatios[event.target.value];
        app.ports.changeAspectRatio.send(aspectRatio);
    });
});
