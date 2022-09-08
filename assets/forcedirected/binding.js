const forcedirectedBinding = new Shiny.OutputBinding();
const forcedirected_plot_by_element = {}

$.extend(forcedirectedBinding, {
    find: function (scope) {
        return $(scope).find(".forcedirected");
    },
    renderValue: function (el, data) {
        plotDetails = forcedirected_plot_by_element[el.id]
        if (!plotDetails) {
            plotDetails = new ForceDirected(el)
            forcedirected_plot_by_element[el.id] = plotDetails
        }

        plotDetails.loadData(el, data.value)
    }
});

Shiny.outputBindings.register(forcedirectedBinding, "forcedirected");




const repeatedforcedirectedBinding = new Shiny.OutputBinding();
const repeatedforcedirected_plot_by_element = {}

$.extend(repeatedforcedirectedBinding, {
    find: function (scope) {
        return $(scope).find(".repeatedforcedirected");
    },
    renderValue: function (el, data) {
        plotDetails = repeatedforcedirected_plot_by_element[el.id]
        if (!plotDetails) {
            plotDetails = new RepeatedForceDirected(el)
            repeatedforcedirected_plot_by_element[el.id] = plotDetails
        }

        plotDetails.loadData(el, data.value)
    }
});

Shiny.outputBindings.register(repeatedforcedirectedBinding,
    "repeatedforcedirected");




const parliamentBinding = new Shiny.OutputBinding();
const parliament_plot_by_element = {}

$.extend(parliamentBinding, {
    find: function (scope) {
        return $(scope).find(".parliament");
    },
    renderValue: function (el, data) {
        plotDetails = parliament_plot_by_element[el.id]

        if (!plotDetails) {
            let elref = d3.select("#" + el.id);
            plotDetails = new ParliamentControl(elref);
            function selectColorGender(v) {
                if (v == "m") return "darkblue";
                if (v == "f") return "pink";
                return "black";
            }
            plotDetails.setField("id", { hidden: true }); // don't show
            plotDetails.setField("gender", {
                color: selectColorGender, values: ["m", "f", "n"]
            }); // specific colours and order
            //p.setField("year", { values: year_names }); // ordered
            //p.setField("class", { values: class_names }); // ordered
            // add the valid layouts
            plotDetails.addLayout(new ParliamentLayout());
            plotDetails.addLayout(new GridLayout());
            plotDetails.addLayout(new BarLayout());
            plotDetails.addLayout(new ScatterLayout());
            plotDetails.addLayout(new PieChartLayout());
            p.layout_config.x = new SortOrderingNumeric("weight");
            p.layout_config.x = new SortOrderingNumeric("income");
            //p.addLayout(new GraphLayout());
            // default colour field
            plotDetails.setColorField("gender");
            // set the data
            //p.setData(data);
            //p.setGraph(graph); // no graph data yet :(
            // render
            //p.jump();
            //}
            const dataOut = [];
            for (let key in data.value) {
                for (let i in data.value[key]) {
                    if (!dataOut[i]) {
                        dataOut[i] = {};
                    }
                    dataOut[i][key] = data.value[key][i];
                }
            }
            plotDetails.setData(dataOut)
            plotDetails.jump()
        }
    }
});

Shiny.outputBindings.register(parliamentBinding, "parliament");
