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


function agreementColors(v){
            if ( v == "Strongly Disagree" ) return "#8c510a";
            if ( v == "Disagree" ) return "#d8b365";
            if ( v == "Slightly Disagree" ) return "#f6e8c3";
            if ( v == "Neither Agree nor Disagree" ) return "#f5f5f5";
            if ( v == "Slightly Agree" ) return "#c7eae5";
            if ( v == "Agree" ) return "#5ab4ac";
            if ( v == "Strongly Agree" ) return "#01665e";
        }

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
            plotDetails.layout_config.x = new SortOrderingNumeric("weight");
            plotDetails.layout_config.x = new SortOrderingNumeric("income");
            plotDetails.setField( "Q1-I feel awkward and out of place in my school", { values:["Strongly Disagree","Disagree","Agree","Strongly Agree"] , color:agreementColors });
            plotDetails.setField( "Q1-I feel like an outsider (or left out of things) at school", { values:["Strongly Disagree","Disagree","Agree","Strongly Agree"] , color:agreementColors });
            plotDetails.setField( "Q1-I feel like I belong at school", { values:["Strongly Disagree","Disagree","Agree","Strongly Agree"] , color:agreementColors });
            plotDetails.setField( "Q1-I feel lonely at school", { values:["Strongly Disagree","Disagree","Agree","Strongly Agree"] , color:agreementColors });
            plotDetails.setField( "Q1-I make friends easily at school", { values:["Strongly Disagree","Disagree","Agree","Strongly Agree"] , color:agreementColors });  
            plotDetails.setField( "Q1-Other students seem to like me", { values:["Strongly Disagree","Disagree","Agree","Strongly Agree"] , color:agreementColors });  

            plotDetails.setField( "Q5-I can talk to or contact my teacher when I need to", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors });  
            plotDetails.setField( "Q5-It is worth building a good relationship with my teacher because I may be in a class or activity with them in the future", { values:["Strongly Disagree","Disagree","Slightly Disagree","Slightly Agree","Agree","Strongly Agree"] , color:agreementColors });  
            plotDetails.setField( "Q5-My teacher and I have shared goals for my progress and development", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors });  
            plotDetails.setField( "Q5-My teacher cares about me", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors });  
            plotDetails.setField( "Q5-My teacher has a good understanding of my skills and interests", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors });  
            plotDetails.setField( "Q5-My teacher inspires and motivates me", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors });  
            plotDetails.setField( "Q5-My teacher recognises and rewards my efforts", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors });  
            plotDetails.setField( "Q5-My teacher treats me fairly", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors });
            plotDetails.setField( "Q5-My teacher understands any particular needs or pressures I face", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors }); 
            plotDetails.setField( "Q5-Neither my nor my teacher's reputation have made the relationship difficult", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree","Agree","Strongly Agree"] , color:agreementColors }); 

            plotDetails.setField( "Teacher-My communication with this student is highly effective ", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree", "Agree","Strongly Agree"] , color:agreementColors }); 
            plotDetails.setField( "Teacher-Our relationship has a strong 'story' or timeline", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree", "Agree","Strongly Agree"] , color:agreementColors }); 
            plotDetails.setField( "Teacher-I know this student well ", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree", "Agree","Strongly Agree"] , color:agreementColors }); 
            plotDetails.setField( "Teacher-Our relationship is fair and respectful ", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree", "Agree","Strongly Agree"] , color:agreementColors }); 
            plotDetails.setField( "Teacher-We are aligned in purpose and values ", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree", "Agree","Strongly Agree"] , color:agreementColors }); 
            plotDetails.setField( "Teacher-There are opportunities to build our relationship", { values:["Strongly Disagree","Disagree", "Slightly Disagree", "Slightly Agree", "Agree","Strongly Agree"] , color:agreementColors }); 
            
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
