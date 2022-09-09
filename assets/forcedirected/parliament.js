class ParliamentControl {
    constructor( d3_div_element , config={} ){
        // default config
        this.config = {
            svg_resize_margin_width:4,
            svg_resize_margin_height:4,
            width:400,
            height:400,
            axis_size:40,
            margin_top:30,
            margin_right:30,
            margin_bottom:30,
            margin_left:30,
            circle_padding:1.2, // closely packed circles
            circle_collision_radius:0.5, // let people squeeze a bit
            bar_spacing:1.4, // separate the bars a bit.
            scatter_sizing:0.007, // size of scattered circles
            network_sizing:0.01, // size of graph circles
            scale_transition:1000, // time for scales to show/hide
            auto_size:true,
            show_controls:false,
            ...config
        };
        this.computeConfig();
        // d3 elements
        this.root_element = d3_div_element;
        this.svg_element = d3_div_element.append("svg").attr("class","parliament").attr("tabindex",0);
        this.svg_element.on("keydown", (e)=>this.onKeydown(e) );
        this.svg_group_edge= this.svg_element.append("g").attr("class","edge");
        this.svg_group_data = this.svg_element.append("g").attr("class","data");
        this.svg_group_scales = this.svg_element.append("g").attr("class","scales");
        this.scales_x = this.createScaleGroup("horizontal");
        this.scales_y = this.createScaleGroup("vertical");
        // legend elements
        this.legend_element = d3_div_element.append("div").attr("class","legend");
        // data based information
        this.data = [];
        this.graph = [];
        // field information
        this.fields = {};
        this.active_fields = [];
        this.graph_fields = {
            "source":{ ignore:true },
            "target":{ ignore:true },
        };
        this.active_graph_fields = [];
        this.active_graph_fields_scalar = [];
        // data filters:
        this.filterF = (d)=>true;
        // potential sort orders:
        this.sort_options = [];
        this.sort_categorical_options = [];
        // possible layouts
        this.layouts = [];
        this.active_layout = null;
        // default layout config:
        this.layout_config = { sort:new SortOrderingStrings("id") };
        // layout control form
        this.layout_control = new LayoutControlForm( this , );
        // setup resizer
        if ( this.config.auto_size ){
            new ResizeObserver( ()=>this.triggerResize() ).observe(d3_div_element.node());
            this.triggerResize();
        }
        this.showControls( this.show_controls );
    }

    onKeydown( event ){
        const key = event.key;
        if ( key == "Escape" ){
            // toggle display of controls.
            this.showControls( !this.controlsVisible );
        }
    }

    showControls( shouldBeVisible ){
        this.controlsVisible = shouldBeVisible;
        this.layout_control.setVisible( this.controlsVisible );
    }

    createScaleGroup( direction ){
        const holder = this.svg_group_scales.append("g").attr("class",direction).attr("font-family","sans-serif");
        const axis = holder.append("g").attr("class","axis");
        const title = holder.append("text").attr("class","title").attr("dominant-baseline","middle").attr("text-anchor","middle");
        return {
            holder:holder,
            axis:axis,
            title:title
        };
    }

    addLayout( layout ){
        this.layouts.push( layout );
        if ( !this.active_layout ){
            this.setActiveLayout( layout );
        }
        this.layout_control.update();
    }
    setActiveLayout( layout ){
        this.active_layout = layout;
        this.layout_control.update();
    }

    setData( data ){
        this.data = data;
        // Process the fields
        this.processFields( data , (key)=>this.getOrCreateField(key) , (f)=>this.fieldsForEach(f) );
        // update the list of active fields
        clearArray(this.active_fields);
        this.fieldsForEach( d=>{ if (d.active && !d.hidden ) this.active_fields.push(d); } );
        this.createElements();
        this.layout_control.update();
        // setup sort options:
        clearArray(this.sort_options);
        clearArray(this.sort_categorical_options);
        this.active_fields.forEach( (f)=>{
            if ( f.type== "number" ){
                const sorter = new SortOrderingNumeric(f.id);
                sorter.key = f.id;
                sorter.title = f.id;
                this.sort_options.push( sorter );
                const sorterReversed = new SortOrderingNumeric(f.id, true );
                sorterReversed.key = f.id+"-rev";
                sorterReversed.title = f.id+" (reversed)";
                this.sort_options.push( sorterReversed );
            }else{
                const sorter =  new SortOrderingStrings(f.id,f.values);
                sorter.key = f.id;
                sorter.title = f.id;
                this.sort_options.push(sorter );
                this.sort_categorical_options.push(sorter );
            }
        });
    }
    setGraph( graph ){
        this.graph = graph;
        // Process the fields
        this.processFields( graph , (key)=>this.getOrCreateGraphField(key) , (f)=>this.graphFieldsForEach(f) );
        // update the list of active fields
        clearArray(this.active_graph_fields);
        clearArray(this.active_graph_fields_scalar);
        this.graphFieldsForEach( d=>{ if (d.active && !d.hidden) this.active_graph_fields.push(d); } );
        this.graphFieldsForEach( d=>{ if (d.active && !d.hidden && d.type=="number") this.active_graph_fields_scalar.push(d); } );
        this.layout_control.update();
    }
    processFields( data , getOrCreate , fieldsForEach ){
        // parse the fields and work out how to use them.
        fieldsForEach( d=>d.active=false );
        data.forEach( d=>{
            const keys = Object.keys(d);
            keys.forEach( (key)=>{
                const f = getOrCreate(key);
                if ( !f.ignore ){
                    f.active = true;
                    const v = d[key];
                    if ( v!= null && v != undefined ){
                        const v_type = typeof v;
                        if ( f.type == undefined ){
                            f.type = v_type;
                        }else{
                            if (f.type != v_type ){
                                throw new Error(`Field contains mixed types: ${key} at ${JSON.stringify(d)}`);
                            }
                        }
                        if ( f.type == "string" ){
                            if ( f.values.indexOf(v) == -1 ){
                                f.values.push(v);
                            }
                        }else if ( f.type== "number" ){
                            if ( !Number.isNaN(v) ){
                                if ( f.max_value < v || f.max_value==undefined){
                                    f.max_value = v;
                                }
                                if ( f.min_value > v || f.min_value==undefined){
                                    f.min_value = v;
                                }
                            }
                        }
                    }
                }
            });
        });
    }
    setField( field_name, details ){
        const current = this.getOrCreateField(field_name);
        this.fields[field_name] = { ...current , ...details };
    }

    getOrCreateField( field_name ){
        const f = this.fields[field_name];
        if ( f ){
            return f;
        }else{
            this.fields[field_name] = { id:field_name, 
                    hidden:false, 
                    type:undefined,
                    values:[], 
                    max_value:undefined, 
                    min_value:undefined,
                    color:undefined, 
                    key:field_name,
                    title:field_name };
            return this.fields[field_name];
        }
    }
    getOrCreateGraphField( field_name ){
        const f = this.graph_fields[field_name];
        if ( f ){
            return f;
        }else{
            this.graph_fields[field_name] = { id:field_name, 
                    hidden:false, 
                    type:undefined,
                    values:[], 
                    max_value:undefined, 
                    min_value:undefined,
                    color:undefined,  
                    key:field_name,
                    title:field_name };
            return this.graph_fields[field_name];
        }

    }

    fieldsForEach( f ){
        const keys = Object.keys(this.fields);
        keys.forEach( key=>f(this.fields[key]) );
    }

    graphFieldsForEach( f ){
        const keys = Object.keys(this.graph_fields);
        keys.forEach( key=>f(this.graph_fields[key]) );
    }

    createElements(){
        this.svg_group_data.selectAll("path").data(this.data).join(
            enter => enter.append("path")
                  .attr("class","data_element").attr("fill",this.data_fill),
                update => update,
                exit => exit
                  .remove());
    }

    setColorField( field ){
        const f = this.fields[field];
        if ( !f ) throw new Error(`Unable to color by unknown field ${field}`);
        if ( !f.color ){
            if (f.type == "number" ){
                const colors = d3.scaleSequential(d3.interpolateViridis);
                f.color = (v)=>colors( (v-f.min_value)/(f.max_value-f.min_value) );
            }else{
                f.color = d3.scaleOrdinal(d3.schemeAccent);
            }
        }
        const c = f.color;
        this.data_fill = (d)=>c(d[field]);
        this.svg_group_data.selectAll("path").transition(500).attr("fill",this.data_fill);
        this.updateLegend( f );
    }

    updateLegend( field ){
        let data = [];
        if ( field.values && (field.values.length > 0) ){
            data = field.values.map( (v)=>{
                return { color:field.color(v), title:v };
            });
        }else{
            const steps = 5;
            const delta = (field.max_value-field.min_value)/(steps-1);
            for (let i=0;i<steps;i++){
                const v = Math.floor(field.min_value + (delta*i));
                data.push( {color:field.color(v), title:v });
            }
        }
        function legendElementCreate(s){
            s.append("span").attr("class","swatch").attr("style",d=>`background-color: ${d.color}`);
            s.append("span").attr("class","title").text(d=>d.title);
        }
        function legendElementUpdate(s){
            s.select("span.swatch").attr("style",d=>`background-color: ${d.color}`);
            s.select("span.title").text(d=>d.title);
        }
        this.legend_element.selectAll( "div" ).data( data ).join(
            enter=>enter.append("div").call( legendElementCreate ),
            update=>update.call( legendElementUpdate ),
            exit=>exit.remove()
        );
    }

    triggerResize(){
        const node = this.root_element.node();
        const width = Math.max(10,node.offsetWidth - this.config.svg_resize_margin_width);
        const height = Math.max(10,node.offsetHeight - this.config.svg_resize_margin_height);
        this.config.width = width;
        this.config.height = height;
        this.computeConfig();
        this.svg_element
            .attr("width",width+"px")
            .attr("height",height+"px")
            .attr("viewBox",`0 0 ${width} ${height}`);
        this.transition();
        this.rerender();
    }
    computeConfig(){
        const config = this.config;
        config.inner_width = config.width - config.margin_left - config.margin_right;
        config.inner_height = config.height - config.margin_top - config.margin_bottom;
        config.center_x = config.inner_width/2 + config.margin_left;
        config.center_y = config.inner_height/2 + config.margin_top;
    }

    jump(){
        if ( !this.active_layout ) return;
        this.applyTargetPoints( true );
        this.updateScales();
        // we're jumping so just set up the positions
        this.rerender();
    }
    transition(){
        if ( !this.active_layout ) return;
        this.applyTargetPoints( false );
        this.updateScales();
        const filtered_nodes = this.data.filter( d=>!d.hidden );
        if ( !this.simulation ){
            this.simulation = d3.forceSimulation();
            console.log("Simulation created");
        }
        this.simulation.nodes(filtered_nodes);
        if ( this.active_layout.useAnchorLinks() ){
            this.simulation.force("move-towards", wanderTowardsForce() );
        }else{
            this.simulation.force("move-towards",null);
        }
        if ( this.active_layout.useCollision() ){
            this.simulation.force('collision', d3.forceCollide().radius( this.active_layout.getCollisionRadiusAccessor(this.config.circle_collision_radius) ));
        }else{
            this.simulation.force('collision',null);
        }
        if ( this.active_layout.useCharge() ){
            this.simulation.force('charge', d3.forceManyBody());
        }else{
            this.simulation.force('charge',null);
        }
        if ( this.active_layout.useEdgeLinks() ){
            const force = d3.forceLink().id(d=>d.id);
            this.active_layout.applyEdges(force,this.data,this.graph,this.layout_config,this.config,filtered_nodes);
            this.simulation.force("link", force);
            if ( this.active_layout.drawEdgeLinks() ){
                this.createEdgeLinks( force.links() );
            }else{
                this.createEdgeLinks( [] );
            }
        }else{
            this.simulation.force("link",null);
            this.createEdgeLinks( [] ); // no links to render
        }
        if ( this.active_layout.useCenterForce() ){
            this.simulation.force('center', d3.forceCenter( this.config.center_x , this.config.center_y ).strength(0.2) );
        }else{
            this.simulation.force("center",null);
        }
        this.simulation.alpha(1.0).alphaMin(0.01).velocityDecay( 0.8 ) ; // initial kick
        this.simulation
            .on("end", ()=>{ this.simulation=null; console.log("Simulation ended"); } )
            .on("tick", ()=>this.rerender() )
            .on("tick.reduce", ()=>{
                // determine how ar we are from ideal.
                let max_delta = 0.0;
                let max_speed_squared = 0.0;
                filtered_nodes.forEach( (d,i)=>{
                    if (! d.hidden ){
                        const dx = d.x - d.tx;
                        const dy = d.y - d.ty;
                        if ( dx && dy ){
                            max_delta = Math.max( max_delta , dx , dy );
                        }
                        const speed_squared = (d.vx*d.vx) + (d.vy*d.vy);
                        if ( speed_squared > max_speed_squared ){
                            max_speed_squared = speed_squared;
                        }
                    }
                });
                if ( this.active_layout.expectExactPosition() ){
                    if ( max_delta > 3.0 ){
                        //console.log("Running")
                        this.simulation.alpha(0.4); // keep running if positions aren't done
                    }else if (max_delta > 1.0 ){
                        //console.log("Close")
                        this.simulation.alpha( 0.04 );
                    }else{
                        console.log("Done " + max_delta);
                        this.simulation.alpha( 0.0);
                        // force all points ot the final location and rerender
                        filtered_nodes.forEach( (d,i)=>{
                            d.vx =0;
                            d.vy = 0;
                            d.x = d.tx;
                            d.y = d.ty;
                        });
                        this.rerender();
                    }
                } else{
                    const max_speed = Math.sqrt(max_speed_squared);
                    if ( max_speed > 1.0 ){
                        this.simulation.alpha(0.2); // keep running if things are in motion
                    }
                }
            } );
    }
    createEdgeLinks(edges){
        this.svg_group_edge.selectAll("path").data(edges).join(
            enter => enter.append("path").attr("class","edge_element").attr("opacity",0).transition(100).attr("opacity",1),
            update => update,
            exit => exit.remove());
    }
    rerender(){
        if ( !this.active_layout ) return;
        this.svg_group_data.selectAll("path").attr("class","data_element").attr("title",d=>d.id)
            .call( (e)=>this.active_layout.applyDataElement(e,false) );
        if ( this.active_layout.drawEdgeLinks() ){
            this.svg_group_edge.selectAll("path")
                .call( (e)=>this.active_layout.applyEdgeElement(e,false) );
        }
    }
    updateScales(){
        const layout_details = this.active_layout.getScaleDetails();
        this.updateScale( layout_details.x , this.scales_x );
        this.updateScale( layout_details.y , this.scales_y );
    }
    updateScale( details , svg_group ){
        if ( details ){
            svg_group.holder.transition(this.config.scale_transition).attr("opacity",1);
            let axis = null, position = null, text_transform=null, scaleMidPoint=0;
            if ( details.scale ){
                const range = details.scale.range();
                scaleMidPoint = (range[0]+range[1]) / 2;
            }
            switch( details.position ){
                case "left":
                default:
                    axis = d3.axisLeft();
                    position = `translate(${this.config.margin_left+this.config.axis_size} 0)`;
                    text_transform =  `translate(${-this.config.axis_size} ${scaleMidPoint}) rotate(90)`;
                    break;
                case "right":
                    axis = d3.axisRight();
                    position = `translate(${this.config.width-this.config.margin_right+this.config.axis_size} 0)`;
                    text_transform =  `translate(${-this.config.axis_size} ${scaleMidPoint}) rotate(90)`;
                    break;
                case "top":
                    axis = d3.axisTop();
                    position = `translate(0 ${this.config.margin_top+this.config.axis_size})`;
                    text_transform =  `translate(${scaleMidPoint} ${this.config.axis_size})`;
                    break;
                case "bottom":
                    axis = d3.axisBottom();
                    position = `translate(0 ${this.config.height-this.config.margin_bottom-this.config.axis_size})`;
                    text_transform =  `translate(${scaleMidPoint} ${this.config.axis_size})`;
                    break;
            }
            svg_group.holder.attr("transform",position);
            if ( details.scale ){
                axis.scale(details.scale);
                svg_group.axis.transition(this.config.scale_transition).call(axis);
                svg_group.title.transition(this.config.scale_transition).attr("transform",text_transform);
            }
            if ( details.title ){
                svg_group.title.text( details.title );
            }else{
                svg_group.title.text( "" );
            }
        }else{
            svg_group.holder.transition(this.config.scale_transition).attr("opacity",0);
        }
    }
    applyTargetPoints( update_data_elements ){
        const filteredData = this.data.filter( d=>this.filterF(d) );
        const dataIds = filteredData.map( d=>d.id );
        const exists = (d)=>dataIds.indexOf(d)!=-1;
        const graphData = this.graph.filter( e=>exists(e.source)&&exists(e.target ));
        const targets = this.active_layout.process(filteredData,graphData,this.layout_config,this.config);
        if ( !targets ) return;
        // convert into map...

        const position_map = {};
        targets.forEach( d=>{ position_map[d.id] = d; });
        let matched = 0;
        let missing = 0;
        let unmatched = 0;
        this.data.forEach( (d,i)=>{
                const p = position_map[d.id];
                if ( p ){
                    matched ++;
                    if ( !Number.isNaN(p.x) && !Number.isNaN(p.y) ){
                        d.tx = p.x;
                        d.ty = p.y;
                        d.hidden = p.hidden || false;
                        if ( update_data_elements ){
                            d.x = p.x;
                            d.y = p.y;
                        }
                    }else{
                        missing ++;
                        d.tx = d.x;
                        d.ty = d.y;
                        d.hidden = true;
                    }
                }else{
                    unmatched ++;
                    d.hidden = true; // hidden as we don't have a data point
                }
        });
        console.log(`applyTargetPoints input:${this.data.length} filtered:${filteredData.length} positions:${targets.length}  matched:${matched}   unmatched: ${unmatched}   missing:${missing}`);
        this.svg_group_data.selectAll("path").transition(500)
            .attr("opacity", (d)=>d.hidden?0.0:1.0 )
            .call( (e)=>this.active_layout.applyDataElement(e,true) );
    }
}


class LayoutControlForm{
    constructor(parliament){
        this.parliament = parliament;
        this.element = parliament.root_element.append("div").attr("class","parliament-controls");
        this.filter_div = this.element.append("div").attr("class","layout-filter");
        this.main_div = this.element.append("div").attr("class","main-controls");
        // firstly we have a drop down for the styling as this is common:
        this.style_dropdown = new DropdownControl( this.main_div , parliament.active_fields , "color" , "Color:" , ()=>this.colorChanged() );
        // next we have a drop down for the layouts:
        this.layout_dropdown = new DropdownControl( this.main_div , parliament.layouts , "layout" , "Layout:" , ()=>this.layoutChanged() );

        this.filter_div = this.element.append("div").attr("class","layout-filter");
        this.filter_dropdown = new DropdownControl( this.filter_div , parliament.active_fields , "filter" , "Filter:" , ()=>this.filterChanged() );
        this.filter_dropdown.filter = ( p=> p.values.length>0 && p.values.length<20 ); // limit to those with values, and less than 20 of them.
        this.filter_values_div = this.filter_div.append("div").attr("class","layout-filter-values");
        this.filterSelectedOptions = [];
        this.cachedFilters = {};

        this.layout_div = this.element.append("div").attr("class","layout-controls");
        this.layout_controls = {};

        this.final_div = this.element.append("div").attr("class","layout-final");
    }
    setVisible( target ){
        this.element.classed( "hidden",!target );
    }
    update(){
        this.style_dropdown.update();
        this.layout_dropdown.update();
        this.filter_dropdown.update();
        this.layout_dropdown.set( this.parliament.active_layout );
        // now we populate dropdowns based on this layouts settings
        const controls = this.parliament.active_layout.getConfig();
        const keys = Object.keys(controls);
        this.layout_control = {};
        this.layout_div.selectAll("*").remove();
        keys.forEach( (key)=>{
            const control_type = controls[key];
            let options = [];
            switch(control_type){
                case "sort":
                    options = this.parliament.sort_options;
                    break;
                case "sort(categorical)":
                    options = this.parliament.sort_categorical_options;
                    break;
                case "graph_field":
                    options = this.parliament.active_graph_fields;
                    break;
                case "graph_field(scalar)":
                    options = this.parliament.active_graph_fields_scalar;
                    break;
                case "override-sort-direction":
                    options = [ 
                        {title:"Ascending", key:"ASC", value:-1} , 
                        {title:"Normal", key:"NORM", value:0}, 
                        {title:"Descending", key:"DESC", value:1}  ];
                    break;
                default:
                    throw Error(`Unexpected control type found ${control_type} for ${key}`);
            }
            this.layout_controls[key] = new DropdownControl( this.layout_div , options, key , key , ()=>this.layoutSettingsChanged(key) );
            const current_value = this.parliament.layout_config[key];
            if ( current_value ){
                this.layout_controls[key].set( current_value );
            }
        });
    }
    colorChanged(){
        const new_color_field = this.style_dropdown.get();
        if ( new_color_field ){
            this.parliament.setColorField( new_color_field.id );
        }
    }
    filterChanged(){
        const filter_field = this.filter_dropdown.get();
        const values = filter_field.values;
        const key = filter_field.key;
        this.filterSelectedOptions = this.cachedFilters[key]||[...values];
        this.cachedFilters[key] = this.filterSelectedOptions;
        const filters = this.filterSelectedOptions;

        const updateFilters = filters=>{
            this.parliament.filterF = (v)=>filters.indexOf(v[key])!=-1;
            this.parliament.transition();
        };

        function filterChanged(event){
            const checkbox = event.target;
            const value = checkbox.__data__;
            const checked = checkbox.checked;
            const currentChecked = filters.indexOf(value)>=0;
            if ( checked != currentChecked ){
                if ( checked ){
                    filters.push( value );
                }else{
                    const index = filters.indexOf(value);
                    filters.splice( index , 1 );
                }
            }
            updateFilters( filters );
        }

        function createDiv( selection ){
            selection.append("label")
                .attr('for',(d,i)=>'filter-option-'+i);
            selection.append("input")
                .attr("type", "checkbox")
                .attr("id", (d,i)=>'filter-option-'+i)
                .on("change", (event)=>filterChanged(event) );
        }
        function setupDiv( selection ){
            selection.attr("title",d=>d);
            selection.selectAll("label")
                .text( d=>d );
            selection.selectAll("input")
                .attr("checked", (d)=>filters.indexOf(d)>=0?true:null )
                .on("change", (event)=>filterChanged(event) );
        }

        this.filter_values_div.selectAll("div").remove();
        this.filter_values_div.selectAll("div")
            .data(values)
            .join( 
                enter=>enter.append("div").call( createDiv ),
                update=>update,
                exit=>exit.remove() );

        this.filter_values_div.selectAll("div").call( setupDiv );
        
        updateFilters( filters );
    }
    layoutChanged(){
        const new_layout = this.layout_dropdown.get();
        if ( new_layout ){
            this.parliament.active_layout = new_layout;
            this.parliament.transition();
            this.update(); // update to the latest fields
        }
    }
    layoutSettingsChanged(key){
        const controls = this.parliament.active_layout.getConfig();
        const keys = Object.keys(controls);
        keys.forEach( (key)=>{
            const value = this.layout_controls[key].get();
            if ( value ){
                this.parliament.layout_config[key] = value;
            }
        });
        this.parliament.transition();
    }
}

class DropdownControl {
    constructor( parent , data , title , label=null, changeF=()=>{} , filterF=()=>true ){
        this.parent = parent;
        this.filter = filterF;
        if ( label != null ){
            this.parent
            .append('label')
            .text( label )
            .attr("for", `${title}-list-id`);
        }
        this.elem = this.parent
            .append('select')
            .attr("id", `${title}-list-id`)
            .attr("name", `${title}-list`)
            .on("change",changeF);
        this.setData( data );
    }
    setData(data){
        this.data = data;
        data.forEach( d=>{
            if (!d.key) throw Error(`Drop down field without a key ${JSON.stringify(d)}`);
        });
        this.update();
    }
    setFilter(filterF){
        this.filter = filterF;
        this.update();
    }
    update(){
        const disabled = d=>(this.filter(d))?null:"true";
        this.elem.selectAll('option').data( this.data ).join(
            enter => enter.append("option")
              .text(d=>d.title)
              .attr("value",d=>d.key)
              .attr("disabled",disabled),
            update => update
              .text(d=>d.title)
              .attr("value",d=>d.key)
              .attr("disabled",disabled),
            exit => exit
              .remove());
    }
    get(){
        const value = this.elem.node().value;
        for ( let index in this.data ){
            const t = this.data[index];
            if ( this.filter(t) && t.key == value ){
                return t;
            }
        }
        return null;
    }
    set( target ){
        if ( typeof target == "string" ){
            this.elem.node().value = target;
        }else{
            this.elem.node().value = target.key;
        }
    }
}

class SortOrderingBase {
    sortAndReturnIds(input){
        return [...input].filter( d=>this.filter(d) ).sort( (a,b)=>this.compare(a,b) ).map( d=>d.id );
    }
    compare(a,b){
        return this.compareKeys(this.f(a),this.f(b) );
    }
    compareKeys(va,vb){
        return this.reversed?va-vb:vb-va;
    }
    filter(d){
        const v = this.f(d);
        return v!==null && v!==undefined && !Number.isNaN(v);
    }
}
class SortOrderingNumeric extends SortOrderingBase {
    constructor(accessor , reversed=false){
        super();
        const t =typeof accessor;
        this.reversed = reversed;
        if ( t === "function" ){
            this.f = accessor;
            this.name = "magic";
        }else if ( t === "string" ){
            this.f = (d)=>d[accessor];
            this.name = accessor;
        }else{
            throw new Error(`Unknown accessor type: ${t} (${accessor})`);
        }
    }
}

class SortOrderingStrings extends SortOrderingNumeric {
    constructor(accessor, values, reversed=false){
        super(accessor,reversed);
        this.values = values;
    }
    compare(a,b){
        const va = this.f(a);
        const vb = this.f(b);
        return this.compareKeys(va,vb);
    }
    compareKeys(va,vb){
        if ( va === vb ){
            return 0;
        }else{
            let answer = va < vb ? -1 : 1;
            if ( this.values ){
                answer = this.values.indexOf(va) - this.values.indexOf(vb); 
            }
            if ( this.reversed ){
                return -answer;
            }else{
                return answer;
            }
        }
    }
    groupAndReturn(data){
        const output = [];
        const keyed = {};
        data.forEach( (d)=>{
            const key = this.f(d);
            if ( !keyed[key] ){
                keyed[key]={ key:key , data:[] };
                output.push( keyed[key] );
            }
            keyed[key].data.push(d);
        });
        return output;
    }
}
class SortOrderingCombine extends SortOrderingBase{
    constructor(first,second){
        super();
        this.first = first;
        this.second = second;
    }
    compare(a,b){
        const f = this.first.compare(a,b);
        if ( f != 0 ) return f;
        return this.second.compare(a,b);
    }
    filter(d){
        return this.first.filter(d) && this.second.filter(d);
    }
}

class RangeComputer{
    constructor(){
        this.min = undefined;
        this.max = undefined;
        this.mid = undefined;
        this.range = undefined;
    }
    include(v){
        if ( this.min===undefined || this.min > v ){
            this.min = v;
            this.compute();
        }
        if ( this.max===undefined || this.max < v ){
            this.max = v;
            this.compute();
        }
    }
    compute(){
        this.range = this.max-this.min;
        this.mid = this.min + (this.max - this.min)/2;
    }
    toScaleDetails( details , minV , maxV ){
        details.scale = this.toScale( minV , maxV );
    }
    toScale( minV , maxV ){
        return d3.scaleLinear()
            .domain([this.min,this.max])
            .range([minV, maxV]);
    }
    computeScale( width ){
        return width / this.range;
    }
}
class CategoricalRangeComputer{
    constructor( compareF = (a,b)=>0 ){
        this.values = [];
        this.counts = {};
        this.compareF = compareF;
    }
    include(v){
        if ( this.values.indexOf(v) == -1 ){
            this.values.push(v);
            this.counts[v] = 1;
        }else{
            this.counts[v] ++;
        }
    }
    scaleToFit( minV , maxV ){
        const tRange = (maxV-minV)/this.values.length;
        return (v)=> ((0.5+this.values.indexOf(v))*tRange)+minV;
    }
    toScaleDetails( details , minV , maxV ){
        details.scale = this.toScale( minV , maxV );
    }
    toScale( minV , maxV ){
        return d3.scalePoint()
            .range([minV,maxV])
            .domain( this.values.sort( this.compareF ) )
            .padding( 0.2 )
            .round(true);
    }
}


class CircleLayout {
    constructor(){
        this.radius = 9; // default radius value
    }
    applyDataElement(e,updateShape){
        e.attr("transform",d=>`translate(${d.x} ${d.y})`);
        if ( updateShape ){
            e.transition(1000).attr("d",(d,i)=>this.makeCircle( d.hidden?0:this.radius ));
        }
    }
    makeCircle(radius){
        if ( !radius ){
            radius = 0;
        }
        return `M ${-radius}, 0  a ${radius},${radius} 0 1,0 ${radius*2},0 a ${radius},${radius} 0 1,0 ${radius*-2},0`;
    }
    getCollisionRadiusAccessor( factor ){
        return (d,i)=>d.hidden?0:(this.radius*factor);
    }
    expectExactPosition(){
        return true;
    }
    useAnchorLinks(){
        return true;
    }
    useCollision(){
        return true;
    }
    useCharge(){
        return false;
    }
    useEdgeLinks(){
        return false;
    }
    drawEdgeLinks(){
        return false;
    }
    useCenterForce(){
        return false;
    }
    getScaleDetails(){
        return {};
    }
}

class GridLayout extends CircleLayout{
    constructor(){
        super();
        this.key = "grid";
        this.title = "Grid";
    }
    process( data , graph , config , parliament_config ){
        const sort = config.sort;
        const sortedIds = sort.sortAndReturnIds( data );
        const elementCount = sortedIds.length;
        const sideLength = Math.ceil( Math.sqrt(elementCount));
        const widthBasedSize = parliament_config.inner_width / (sideLength+1);
        const heightBasedSize = parliament_config.inner_height / (sideLength+1);
        const circle_spacing = Math.min( widthBasedSize , heightBasedSize );
        const circle_radius = circle_spacing / parliament_config.circle_padding / 2.0;
        this.radius = circle_radius;
        return sortedIds.map( (d,i)=>{
            const x_pos = i % sideLength - (sideLength/2);
            const y_pos = Math.floor(i / sideLength) - (sideLength/2);
            return {
                x:parliament_config.center_x + x_pos * circle_spacing,
                y:parliament_config.center_y + y_pos * circle_spacing,
                id:d
            };
        });
    }
    getConfig(){
        return {
            "sort":"sort"
        };
    }
}

class ScatterLayout extends CircleLayout{
    constructor(){
        super();
        this.key = "scatter";
        this.title = "Scatter";
        this.clustering = false;
        this.scaleDetails = { 
            x:{ position:"bottom", scale:null } , 
            y:{ position:"left", scale:null } 
        };
    }
    expectExactPosition(){
        return !this.clustering;
    }
    useCollision(){
        return this.clustering;
    }
    getScaleDetails(){
        return this.scaleDetails;
    }
    getCollisionRadiusAccessor( factor ){
        return (d,i)=>d.hidden?0:(this.radius); // we just use the radius when clustering...
    }
        
    process( data , graph , config , parliament_config ){
        this.radius = [];
        if ( !config.x || !config.y ){
            return;
        }
        const x = config.x.f;
        const y = config.y.f;
        
        if ( data.length == 0 ) return []; // no data - ignore everything...
        const typeX = typeof x(data[0]);
        const typeY = typeof y(data[0]);

        let rangeX = null;
        let rangeY = null;
        this.clustering = false;
        if ( typeX == "number" ){
            rangeX = new RangeComputer();
        }else if (typeX == "string"){
            rangeX = new CategoricalRangeComputer( (a,b)=>config.x.compareKeys(a,b) );
            this.clustering = true;
        }else{
            throw new Error(`Unexpected type from field X: ${typeX} ${x}`);
        }
        if ( typeY == "number" ){
            rangeY = new RangeComputer();
        }else if (typeY == "string"){
            rangeY = new CategoricalRangeComputer( (a,b)=>config.y.compareKeys(a,b) );
            this.clustering = true;
        }else{
            throw new Error(`Unexpected type from field Y: ${typeX} ${x}`);
        }


        const positions = data.map( (d,i)=>{
            const x_val = x(d);
            const y_val = y(d);
            if ( Number.isNaN(x_val) || Number.isNaN(y_val) ){
                return { x:d.x , y:d.y, id:d.id, hidden:true };
            }else{
                rangeX.include( x_val );
                rangeY.include( y_val );
                return { x:x_val , y:y_val, id:d.id };
            }
        });
        // now apply scaling.
        rangeX.toScaleDetails( this.scaleDetails.x , parliament_config.margin_left + parliament_config.axis_size , parliament_config.margin_left + parliament_config.inner_width );
        this.scaleDetails.x.title = config.x.title;
        rangeY.toScaleDetails( this.scaleDetails.y , parliament_config.margin_top + parliament_config.inner_height - parliament_config.axis_size ,  parliament_config.margin_top );
        this.scaleDetails.y.title = config.y.title;
        positions.forEach( (d)=>{
            if ( !d.hidden ){
                // we don't want to move hidden ones...
                d.x = this.scaleDetails.x.scale(d.x);
                d.y = this.scaleDetails.y.scale(d.y);
            }
        });
        this.radius = Math.max(parliament_config.inner_width,parliament_config.inner_height)*parliament_config.scatter_sizing;
        return positions;
    }
    getConfig(){
        return {
            "x":"sort",
            "y":"sort"
        };
    }
}


class BarLayout extends CircleLayout{
    constructor(){
        super();
        this.key = "bar";
        this.title = "Bar chart";
        this.scaleDetails = { 
            x:{ position:"bottom", scale:null } , 
            y:false
        };
    }
    getScaleDetails(){
        return this.scaleDetails;
    }
    process( data , graph , config , parliament_config ){
        const bars = config.bars;
        const sort = config.sort;
        if ( !bars || !sort ) return null;
        const groups = bars.groupAndReturn( data );
        const output = [];
        if ( config.sortBarsByHeight.value == 0){
            groups.sort( (a,b)=>bars.compareKeys(a.key,b.key) ); // sort bars by classic order
        }else{
            groups.sort( (a,b)=>config.sortBarsByHeight.value * (b.data.length - a.data.length) ); // sort bars by height
        }
        const tallestBarSize = Math.max( ...groups.map( g=>g.data.length ) );
        const sideLength = Math.ceil( Math.sqrt(tallestBarSize / groups.length ));

        const widthBasedSize = parliament_config.inner_width / (sideLength* groups.length * parliament_config.bar_spacing );
        const heightBasedSize = (parliament_config.inner_height - parliament_config.axis_size) / Math.ceil(tallestBarSize/sideLength);
        const circle_spacing = Math.min( widthBasedSize , heightBasedSize );
        const circle_radius = circle_spacing / parliament_config.circle_padding / 2.0;
        this.radius = circle_radius;

        // axis
        const scaleX = d3.scalePoint()
                    .range([parliament_config.margin_left,parliament_config.margin_left+parliament_config.inner_width])
                    .domain( groups.map(g=>g.key) )
                    .padding( 0.4 ) // note, large because the bars might be wide
                    .round(true);
        this.scaleDetails.x.scale = scaleX;
        this.scaleDetails.x.title = config.bars.title;

        groups.forEach( (group)=>{
            const sortedIds = sort.sortAndReturnIds( group.data );
            const bar_position = scaleX(group.key) - ((sideLength-1)*circle_spacing)/2;
            sortedIds.forEach( (d,i)=>{
                const x_pos = i % sideLength;
                const y_pos = Math.floor(i / sideLength);
                output.push({
                    x: bar_position + x_pos * circle_spacing,
                    y: parliament_config.height - parliament_config.margin_bottom - parliament_config.axis_size - circle_spacing - y_pos * circle_spacing,
                    id:d
                });
            });
        });
        return output;
    }
    getConfig(){
        return {
            "bars":"sort(categorical)",
            "sort":"sort",
            "sortBarsByHeight":"override-sort-direction"
        };
    }
}

class ParliamentLayout extends CircleLayout{
    constructor(){
        super();
        this.key = "parliament";
        this.title = "Parliament";
    }
    process( data , graph , config , parliament_config ){
        const sort = config.sort;
        const sortedIds = sort.sortAndReturnIds( data );
        
        let currentRadius = 10; // should be preset
        let currentPosition = 0;
        const arc_size = 2.0;
        const angle_offset = -(arc_size/2.0);
        const row_spacing = 3.0;
        const col_spacing = 1.0;
        const positions = [];
        while (positions.length < sortedIds.length){
            const spacing = col_spacing*arc_size/currentRadius;
            positions.push({ 
                x: Math.sin(angle_offset+currentPosition)*currentRadius,
                y: -Math.cos(angle_offset+currentPosition)*currentRadius,
                theta:angle_offset+currentPosition } );
            currentPosition += spacing;
            if ( currentPosition >= arc_size ){
                currentRadius += row_spacing;
                const new_spacing = col_spacing*arc_size/currentRadius;
                const remaining =  sortedIds.length - positions.length;
                if ( (arc_size/new_spacing) < remaining ){
                    const extra_space = arc_size % new_spacing;
                    currentPosition = extra_space/2;
                }else{
                    currentPosition = (arc_size-remaining*new_spacing)/2;
                }
            }
        }
        const rangeX = new RangeComputer();
        const rangeY = new RangeComputer();
        positions.forEach( d=>{
            rangeX.include(d.x);
            rangeY.include(d.y);
        });
        const scaleF = Math.min( rangeX.computeScale( parliament_config.inner_width ) , rangeY.computeScale( parliament_config.inner_height ) );
        positions.sort( (a,b)=>a.theta-b.theta );

        const circle_radius = scaleF/parliament_config.circle_padding;
        this.radius = circle_radius;
        const circle_spacing = scaleF;

        return sortedIds.map( (d,i)=>{
            const pos = positions.pop();
            const x_pos = pos.x - rangeX.mid; // recenter the parliament
            const y_pos = pos.y - rangeY.mid;
            return {
                x:parliament_config.center_x + (x_pos * circle_spacing),
                y:parliament_config.center_y + (y_pos * circle_spacing),
                id:d
            };
        });
    }
    getConfig(){
        return {
            "sort":"sort"
        };
    }
}


class PieChartLayout extends CircleLayout{
    constructor(){
        super();
        this.key = "pie-chart";
        this.title = "Pie chart";
    }
    process( data , graph , config , parliament_config ){
        const sort = config.sort;
        const sortedIds = sort.sortAndReturnIds( data );

        // generate some circles...
        const radius = Math.ceil(Math.sqrt(sortedIds.length));

        const magic = Math.sqrt( 3.0 ) / 6.0 - 0.5;
        const circles = [];
        const diameter_circle = 30.0;
        const xoffset = diameter_circle * 0.5;
        const yoffset = diameter_circle * 0.4;
        for( var u = -radius; u <= radius; u++ ) {
            for( var v = -radius; v <= radius; v++ ) {
            
                if( u > v + radius || u < v - radius )
                    continue;

                var offset = ( u + v ) * magic;
                const pos = {
                    x:xoffset+( u + offset ) * ( 1 - magic ) * diameter_circle,  // x
                    y:yoffset+( v + offset ) * ( 1 - magic ) * diameter_circle,  // y
                };
                pos.theta = Math.atan2(pos.x,pos.y);
                pos.distance = Math.sqrt( (pos.x*pos.x) + (pos.y*pos.y) );
                circles.push(pos);
            }
        }
        // now assign positions
        const output = [];
        const radialCircles = circles.sort( (a,b)=>a.distance-b.distance);
        const finalCircles = radialCircles.splice( 0 , sortedIds.length );
        finalCircles.sort( (a,b)=>a.theta-b.theta );

        const rangeX = new RangeComputer();
        const rangeY = new RangeComputer();
        finalCircles.forEach((d)=>{
            rangeX.include(d.x);
            rangeY.include(d.y);
        });
        const scale = Math.min( 
                rangeX.computeScale(parliament_config.inner_width) , 
                rangeY.computeScale(parliament_config.inner_height) );
        this.radius = 0.5*diameter_circle*scale/parliament_config.circle_padding;
        sortedIds.forEach( id=>{
            const o = finalCircles.splice(0,1)[0];
            o.x = parliament_config.center_x + o.x*scale;
            o.y = parliament_config.center_y + o.y*scale;
            o.id = id;
            output.push(o);
        });

        return output;
    }
    getConfig(){
        return {
            "sort":"sort"
        };
    }
}
class GraphLayout extends CircleLayout{
    constructor(){
        super();
        this.key = "graph";
        this.title = "Graph";
    }
    expectExactPosition(){
        return false;
    }
    useAnchorLinks(){
        return false;
    }
    useCollision(){
        return true;
    }
    useCharge(){
        return true;
    }
    useEdgeLinks(){
        return true;
    }
    drawEdgeLinks(){
        return true;
    }
    useCenterForce(){
        return true;
    }
    process( data , graph , config , parliament_config ){
        const scaleF = Math.min( parliament_config.inner_width, parliament_config.inner_height );
        const circle_radius = Math.ceil(scaleF * parliament_config.network_sizing);
        this.radius = circle_radius;
        this.length = config.length;
        if ( this.length && this.length.type == "number" ){
            this.lengthScale = d3.scaleLinear().range([30,130]).domain( [this.length.min_value,this.length.max_value] );
            const key = this.length.key;
            this.lengthFunction = (d)=>this.lengthScale( d[key] );
        }else{
            this.lengthFunction  = (d)=>100;
        }
        return data.map( (d,i)=>{
            return {
                hidden:false,
                x:d.x,
                y:d.y,
                id:d.id
            };
        });
    }
    applyEdges( force, data , graph , config , parliament_config , active_nodes ){
        console.log("Apply Edges "+active_nodes.length+" nodes are active");
        const active_ids = active_nodes.map( d=>d.id );
        const activeId = (id)=>active_ids.indexOf(id) != -1;
        const active_edges = graph.filter( (e)=>(activeId(e.source)&&activeId(e.target)) );
        const l = this.lengthFunction;
        const edges = active_edges
            .map( (e)=>{ return { source:e.source , target:e.target, length:l(e) }; })
            .filter( d=> !Number.isNaN(d.length) && d.length !== undefined && d.length>0  );
        console.log(` Returning ${edges.length} valid edges: ${edges.length>0?JSON.stringify(edges[0]):null}`);
        force.distance( d=>d.length ).strength(0.3).links(edges);
    }
    applyEdgeElement(selection){
        selection.attr("d",d=>this.makeLine(d.source.x,d.source.y,d.target.x,d.target.y)).attr("stroke","grey");
    }
    makeLine(sx,sy,dx,dy){
        return `M ${sx} ${sy} L${dx} ${dy}`;
    }
    getConfig(){
        return {
            "length":"graph_field(scalar)"
        };
    }
}


/*
 custom force = Head's towards the anchor positions, with some added noise
*/
function wanderTowardsForce() {
    var nodes, movement, initial_target_speed = 30, rotate_squee = 0.1, momentum=0.9;
  
    function force(alpha) {
      var i,
          n = nodes.length,
          node;
      for (i = 0; i < n; ++i) {
        node = nodes[i];
        const initial_target_speed = movement[i].speed;
        const rotate_squee = movement[i].squee;
        const momentum = movement[i].momentum;
        const inverse_momentum = 1.0-momentum;
        if ( node.tx && node.ty && !node.hidden ){
            // get the current offset direction
            const dx = node.tx - (node.x+node.vx);
            const dy = node.ty - (node.y+node.vy);
            const offset_length = Math.sqrt( (dx*dx)+(dy*dy));
            const l = Math.max(0.5,offset_length); // limit this to 0.5 so we don't get tiny speeds and huge forces
            let target_speed = initial_target_speed;
            if ( alpha < 0.1 ){
                target_speed = target_speed * 4.0; // hurry factor
            }
            target_speed = Math.min( target_speed , offset_length ); // don't move faster than remaining distance
            if ( l > 2.0){
                const adx = dx/l;
                const ady = dy/l;
                const tvx = target_speed * (adx + rotate_squee*ady);
                const tvy = target_speed * (ady - rotate_squee*adx); // give a bit of rotation
                node.vx = (momentum*node.vx + inverse_momentum*tvx);
                node.vy = (momentum*node.vy + inverse_momentum*tvy);
            }else{
                // close enough - just pin it in place
                node.x = node.tx;
                node.y = node.ty;
            }
        }
      }
    }
  
    force.initialize = function(_) {
      nodes = _;
      movement = nodes.map( ()=>{
        return {
            speed: 90.0 + (Math.random() * 10.0),
            squee: (Math.random()* 0.4)+0.1,
            momentum: (Math.random()*0.3) + 0.6
        };
      });
    };

  
    return force;
  }

  function safeAccess( obj , key ){
    if ( obj == undefined ) return obj;
    return obj[key];
  }

  function clearArray(target){
    target.splice(0,target.length);
  }