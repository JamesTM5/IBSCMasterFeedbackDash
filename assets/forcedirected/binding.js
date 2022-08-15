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

Shiny.outputBindings.register(repeatedforcedirectedBinding, "repeatedforcedirected");




const parliamentBinding = new Shiny.OutputBinding();
const parliament_plot_by_element = {}

$.extend(parliamentBinding, {
  find: function (scope) {
    return $(scope).find(".parliament");
  },
  renderValue: function (el, data) {
    plotDetails = parliament_plot_by_element[el.id]

    if (!plotDetails) {
      plotDetails = new ParliamentButtons("#"+el.id)
      parliament_plot_by_element[el.id] = plotDetails
    }
    const dataOut = [];
    for ( let key in data.value ){
      for ( let i in data.value[key] ){
        if (!dataOut[i]){
          dataOut[i]={};
        }
        dataOut[i][key] = data.value[key][i];
      }
    }
    plotDetails.setData(dataOut)
    plotDetails.jump()
  }
});

Shiny.outputBindings.register(parliamentBinding, "parliament");
