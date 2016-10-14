window.addEventListener('load', function () {
    var image = document.getElementById('image');
    var node = document.getElementById('main');
    var app = Elm.ImageCrop.Interop.embed(node, {
        image: {width: 900, height: 600},
        cropAreaWidth: getImageWidth(),
        offset: {x: 8, y: 8},
        selection: {topLeft: {x: 0, y: 0}, bottomRight: {x: 100, y: 100}},
        aspectRatio: null
    });
    var
        coordinatesDiv = document.getElementById('coordinates'),
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
            coordinatesDiv.style.display = 'block';
        } else {
            coordinatesDiv.style.display = 'none';
        }
    });
    //var resizeDetector = document.getElementById('resize-detector');
    //resizeDetector.addEventListener('load', function () {
    //    resizeDetector
    //        .contentDocument
    //        .defaultView
    //        .addEventListener('resize', function () {
    //            app.ports.viewportChanged.send(getImageWidth());
    //        });
    //});
    window.addEventListener('resize', function () {
        app.ports.viewportChanged.send(getImageWidth());
    });
    function getImageWidth() {
        return +getComputedStyle(image).width.replace('px', '');
    }
    document
        .getElementById('aspect-ratio-form')
        .addEventListener('submit', function (event) {
            event.preventDefault();
            var aspectRatio = {
                width: +document.getElementById('aspect-ratio-width-input').value,
                height: +document.getElementById('aspect-ratio-height-input').value,
            };
            app.ports.changeAspectRatio.send(aspectRatio);
        });
});
