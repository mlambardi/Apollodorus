# after: https://stackoverflow.com/questions/30527977/ggplot2-how-to-differentiate-click-from-brush

script_click_brush <- "
        $('#plot').mousedown(function(e) {
            var parentOffset = $(this).offset();
            var relX = e.pageX - parentOffset.left;
            var relY = e.pageY - parentOffset.top;
            Shiny.setInputValue('x1', relX);
            Shiny.setInputValue('y1', relY);
        }).mouseup(function(e) {
            var parentOffset = $(this).offset();
            var relX = e.pageX - parentOffset.left;
            var relY = e.pageY - parentOffset.top;
            Shiny.setInputValue('x2', relX);
            Shiny.setInputValue('y2', relY);
            Shiny.setInputValue('action', Math.random());
        });
    "
