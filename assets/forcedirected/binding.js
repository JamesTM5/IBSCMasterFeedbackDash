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
