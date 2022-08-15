class Parliament{
    constructor( svg_reference , config ) {
        this.target = svg_reference;
        this.layout = new GridBoxLayout();
        this.fillFunction = (d)=>"green";
        this.items = 0;
        this.config = {
            squareWidth:8,
            squareHeight:8,
            squareSpacingX:12,
            squareSpacingY:12,
            delayMs:1,
            barSeparation:12,
            barWidth:5,
            width:500,
            height:500,
            margin_left:30,
            margin_right:10,
            margin_top:10,
            margin_bottom:30,
            ...config
        };

        this.scales = {
            xScale:d3.scaleLinear()
                   .domain([0, 2])
                   .range([0,0]),
            yScale:d3.scaleLinear()
                    .domain([0, 2])
                    .range([0,0])
        };
        this.scales.xAxisGenerator = d3.axisBottom(this.scales.xScale);
        this.scales.yAxisGenerator = d3.axisLeft(this.scales.yScale);

        this.squares_g = this.target.append("g")
            .attr("class","squares");
        
        this.scales_g = this.target.append("g")
            .attr("class","scales");
        this.scales_x = this.scales_g.append("g")
            .attr("class","scale_x");
        this.scales_y = this.scales_g.append("g")
            .attr("class","scale_y");
    }
    setData( data ){
        this.data = data;
        console.log("data")
    }
    setLayout( layout ){
        this.layout = layout;
    }
    setColor( fillFunction ){
        this.fillFunction = fillFunction;
    }
    updateConfig(){
        this.config.inner_width = this.config.width - this.config.margin_left - this.config.margin_right;
        this.config.inner_height = this.config.height - this.config.margin_top - this.config.margin_bottom;
        this.config.mid_x = this.config.inner_width/2 + this.config.margin_left;
        this.config.mid_y = this.config.inner_height/2 + this.config.margin_top;
        this.config.position_x = (x)=>this.config.margin_left + x * this.config.inner_width;
        this.config.position_y = (y)=>this.config.margin_top + y * this.config.inner_height;
        this.config.halfSquareWidth = this.config.squareWidth / 2;
        this.config.halfSquareHeight = this.config.squareHeight / 2;
    }
    jump(){
        this.updateConfig();
        const config = this.config;
        this.layout.layout( this.data , config , this.scales );
        this.squares_g
            .selectAll("rect.parliament")
            .data(this.data)
        .join(
            enter => enter.append("rect")
              .attr("class","parliament")
              .attr("fill", this.fillFunction)
              .attr("width", config.squareWidth)
              .attr("height", config.squareHeight)
              .attr("x", d=>d.x)
              .attr("y", d=>d.y)
              .attr("title",d=>JSON.stringify(d)),
            update => update
                .attr("x", d=>d.x)
                .attr("y", d=>d.y),
            exit => exit
              .remove());
        this.scales_x.call(this.scales.xAxisGenerator)
            .attr("transform",`translate(0 ${config.margin_top+config.inner_height})`)
            .attr("opacity",this.layout.getXOpacity());
        this.scales_y.call(this.scales.yAxisGenerator)
            .attr("transform",`translate(${config.margin_left} 0)`)
            .attr("opacity",this.layout.getYOpacity());
    }
    transition(durationMS){
        this.updateConfig();
        const config = this.config;
        this.layout.layout( this.data , config , this.scales );
        this.squares_g
            .selectAll("rect.parliament")
            .data(this.data)
        .join(
            enter => enter.append("rect")
              .attr("class","parliament")
              .attr("fill", this.fillFunction)
              .attr("width", 0)
              .attr("height", 0)
              .attr("x", d=>d.x)
              .attr("y", d=>d.y),
            update => update,
            exit => exit
              .remove());
        this.squares_g
            .selectAll("rect.parliament")
            .transition().duration(durationMS).delay( (d,i)=> (config.delayMs)*i )
              .attr("width", config.squareWidth)
              .attr("height", config.squareHeight)
              .attr("fill", this.fillFunction)
              .attr("x", d=>d.x)
              .attr("y", d=>d.y);
        this.scales_x.transition().duration(durationMS)
            .call(this.scales.xAxisGenerator)
            .attr("transform",`translate(0 ${config.margin_top+config.inner_height})`)
            .attr("opacity",this.layout.getXOpacity());
        this.scales_y.transition().duration(durationMS)
            .call(this.scales.yAxisGenerator)
            .attr("transform",`translate(${config.margin_left} 0)`)
            .attr("opacity",this.layout.getYOpacity());
    }
}

class ParliamentButtons extends Parliament {
    constructor( svg_reference , config ) {
        console.log("bind")
        const target = d3.select(svg_reference);
        const svg = target.append("svg")
        const div = target.append("div");
        super( svg , config )
        svg.attr("width",this.config.width).attr("height",this.config.height);
        this.buttonsDiv = div;
        this.colors = d3.scaleSequential(d3.interpolatePiYG);
        this.categoricalColours = d3.scaleOrdinal(d3.schemeAccent);
    }
    setData( data ){
        super.setData(data);
        // new data creates - set up some display options...
        this.buttons = [ 
            {title:"box co-curricular", activate:()=>{
                this.setLayout( new GridBoxLayout() ); 
                this.layout.sortBy( d=>d["co-curricular"])
                this.setColor( d=>this.categoricalColours(d["co-curricular"]))
                this.transition(1000);
            }},
            {title:"arc co-curricular", activate:()=>{
                this.setLayout( new SemicircleLayout() ); 
                this.layout.sortBy( d=>d["co-curricular"])
                this.setColor( d=>this.categoricalColours(d["co-curricular"]))
                this.transition(1000);
            }},
            {title:"box", activate:()=>{
                this.setLayout( new GridBoxLayout() ); 
                this.layout.sortBy( d=>d["id"])
                this.transition(1000);
            }},
            {title:"arc", activate:()=>{
                this.setLayout( new SemicircleLayout() );
                this.transition(1000);
            }},
        ];
        this.updateButtons();
    }
    updateButtons(){
        const g = this.buttonsDiv.selectAll("button");
        g.data(this.buttons).join(
            enter => enter.append("button")
              .text(d=>d.title)
              .on('click',(event,d)=>{ d.activate() }),
            update => update,
            exit => exit
              .remove());
    }
}

function distanceBetween(a,b){
    if ( a == undefined || b == undefined ){
        throw new Error("Undefined position in distance function")
    }
    const dx = (a.x||0) - (b.x||0);
    const dy = (a.y||0) - (b.y||0);
    return Math.sqrt( dx*dx + dy*dy );
}

class GridBoxLayout{
    constructor(){
        this.sortFunction = (d)=>0;
    }
    sortBy( sortFunction ){
        this.sortFunction = sortFunction;
    }
    getXOpacity(){
        return 0.0;
    }
    getYOpacity(){
        return 0.0;
    }
    layout(data,config,scales){
        const targets = this.layoutPositions(data,config,scales); // get the target positions
        const groupIndexes = {};
        for ( let j=0;j<targets.length;j++){
            const g = targets[j].g;
            if ( ! groupIndexes[g] ){
                groupIndexes[g] = [j];
            }else{
                groupIndexes[g].push(j);
            }
        }
        const sourceGroupIndexes = {};
        for ( let i=0;i<data.length;i++){
            const g = this.sortFunction(data[i]);
            if ( ! sourceGroupIndexes[g] ){
                sourceGroupIndexes[g] = [i];
            }else{
                sourceGroupIndexes[g].push(i);
            }
        }
        for ( let g in groupIndexes ){
            const sourceIndexes = sourceGroupIndexes[g];
            const targetIndexes = groupIndexes[g];
            this.assignPositions( sourceIndexes , targetIndexes , data , targets );
        }
    }
    assignPositions( sourceIndexes , targetIndexes , data , targets ){
        const distances = {};
        // compute the distance matrix
        for (let i=0;i<sourceIndexes.length;i++){
            const data_index = sourceIndexes[i];
            const d = data[data_index];
            const row = {};
            distances[data_index]=row;
            for (let j=0;j<targetIndexes.length;j++){
                const target_index = targetIndexes[j];
                const t = targets[target_index];
                row[target_index] = distanceBetween( t , d); 
            }
        }
        while ( Object.keys(distances).length > 0 ){
            const closest = this.findMinimum( distances );
            const closest_i = closest[0];
            const closest_j = closest[1];
            delete distances[closest_i];
            for ( let n in distances ){
                delete distances[n][closest_j];
            }
            const d = data[closest_i];
            const t = targets[closest_j];
            d.x = t.x;
            d.y = t.y;
        }
    }
    findMinimum( matrix ){
        let bestPair = [-1,-1];
        let bestValue = Number.MAX_VALUE;
        for ( let i in matrix ){
            const row = matrix[i];
            for ( let j in row ){
                const value = row[j];
                if ( value < bestValue ){
                    bestValue = value;
                    bestPair = [i,j];
                }
            }
        }
        return bestPair;
    }
    getSortedGroups(items,data){
        const groups = [];
        for (let i=0;i<items;i++){
            groups.push( this.sortFunction(data[i]) );
        }
        var sortF = undefined;
        if ( typeof(groups[0]) == "number"){
            sortF = (a,b)=>a-b;
        }
        const sortedGroups = groups.sort( sortF );
        return sortedGroups;
    }
    layoutPositions(data,config){
        const items = data.length;
        const sides = Math.floor(Math.sqrt(items));
        const half_side = sides/2;
        const sortedGroups = this.getSortedGroups( items ,data );
        // Here's the ma
        const positions = [];
        for (let i=0;i<items;i++){
            const row = i % sides;
            const col = (i -row)/sides;
            const x = config.mid_x + (row-half_side) * config.squareSpacingX;
            const y = config.mid_y + (col-half_side) * config.squareSpacingY;
            positions.push( { x:x, y:y, g:sortedGroups[i]} );
        }
        return positions;
    }
}

class SemicircleLayout extends GridBoxLayout{
    constructor( config ){
        super();
        this.config = {
            inner_radius:40,
            arc_start_radians:-1,
            arc_end_radians:1,
            ...config
        }
    }
    layoutPositions(data,config){
        const items = data.length;
        // Here's the math's happens to fit them in the shape..
        const sortedGroups = this.getSortedGroups( items , data );
        const positions = [];
        let row = 0;
        let col = 0;
        let arc_total = this.config.arc_end_radians - this.config.arc_start_radians;

        for (let i=0;i<items;i++){
            const radius = (row*config.squareSpacingY)+this.config.inner_radius;
            const col_length = (arc_total * radius) / config.squareSpacingX;
            if ( (col+1) > col_length ){
                col = 0;
                row ++;
            }else{
                col ++;
            }
            const angle = ((col / col_length) * arc_total)+this.config.arc_start_radians;
            const x = config.mid_x + Math.sin(angle) * radius;
            const y = config.mid_y - Math.cos(angle) * radius;
            positions.push( { x:x, y:y , theta:angle} );
        }
        positions.sort( (a,b)=>a.theta-b.theta );
        for (let i=0;i<items;i++){
            positions[i].g = sortedGroups[i];
        }
        return positions;
    }
}

class BarchartLayout extends GridBoxLayout{
    constructor(){
        super();
    }
    getXOpacity(){
        return 1.0;
    }
    getYOpacity(){
        return 0.0;
    }
    layoutPositions(data,config,scales){
        this.barWidthPixels = config.barWidth * config.squareSpacingX + config.barSeparation;
        const counts = {};
        const groups = [];
        const group_positions = [];
        for (let id in data ){
            const d = data[id];
            const g = this.sortFunction(d);
            if( ! counts[g] ){
                counts[g] = 0;
                group_positions.push( config.margin_left  + (groups.length+0.5) * this.barWidthPixels );
                groups.push(g);
            }
            counts[g] = counts[g]+1;
        }
        scales.xScale = d3.scaleOrdinal()
            .domain(groups)
            .range(group_positions);
        scales.xAxisGenerator = d3.axisBottom(scales.xScale);
        const items = data.length;
        const positions = [];
        for (let i=0;i<items;i++){
            const d = data[i];
            const bar = this.sortFunction(d);
            const height = counts[bar] -1; // This is the hight of this block...
            const barX = (height % config.barWidth) - (config.barWidth/2);
            const barY = 1 + ((height - barX)/config.barWidth);
            counts[bar] --;
            const y = config.inner_height + config.margin_top - barY *  config.squareSpacingY;
            const x = scales.xScale(bar) + barX*config.squareSpacingX;
            positions.push( { x:x, y:y, g:bar} );
        }
        return positions;
    }
}


class ScatterchartLayout extends GridBoxLayout{
    constructor(){
        super();
    }
    getXOpacity(){
        return 1.0;
    }
    getYOpacity(){
        return 1.0;
    }
    getSortedGroups(items,data){
        const groups = [];
        for (let i=0;i<items;i++){
            groups.push( this.sortFunction(data[i]) );
        }
        return groups;
    }
    layoutPositions(data,config,scales){
        const items = data.length;
        const positions = [];
        const sortedGroups = this.getSortedGroups( items , data);
        let minX= undefined;
        let minY= undefined;
        let maxX= undefined;
        let maxY= undefined;
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const x_value = parseFloat(parts[0]);
            const y_value = parseFloat(parts[1]);
            if ( x_value < minX || minX===undefined) minX = x_value;
            if ( x_value > maxX || maxX===undefined) maxX = x_value;
            if ( y_value < minY || minY===undefined) minY = y_value;
            if ( y_value > maxY || maxY===undefined) maxY = y_value;
        }
        scales.xScale = d3.scaleLinear()
               .domain([minX, maxX])
               .range([config.margin_left,config.margin_left+config.inner_width]);
        scales.xAxisGenerator = d3.axisBottom(scales.xScale);
        scales.yScale = d3.scaleLinear()
               .domain([minY, maxY])
               .range([config.margin_top+config.inner_height,config.margin_top]);
        scales.yAxisGenerator = d3.axisLeft(scales.yScale);
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const x_value = parseFloat(parts[0]);
            const y_value = parseFloat(parts[1]);
            positions.push( { x:scales.xScale(x_value)-config.halfSquareWidth, y:scales.yScale(y_value)-config.halfSquareHeight, g:group } );
        }
        return positions;
    }
}

class ScatterchartXCategoricalLayout extends ScatterchartLayout{
    constructor(){
        super();
    }
    getXOpacity(){
        return 1.0;
    }
    getYOpacity(){
        return 1.0;
    }
    getSortedGroups(items,data){
        const groups = [];
        for (let i=0;i<items;i++){
            groups.push( this.sortFunction(data[i]) );
        }
        return groups;
    }
    layoutPositions(data,config,scales){
        const items = data.length;
        const positions = [];
        const sortedGroups = this.getSortedGroups( items ,data);
        let minY= undefined;
        let maxY= undefined;
        const groups = [];
        for (let i in sortedGroups){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const g = parts[0];
            if( groups.indexOf(g) == -1 ){
                groups.push(g);
            }
        }
        const group_positions = [];
        const group_width = (config.inner_width / groups.length);
        for (let i in groups){
            group_positions[i] = config.margin_left + group_width*(parseFloat(i)+0.5);
        }
        scales.xScale = d3.scaleOrdinal()
            .domain(groups)
            .range(group_positions);
        scales.xAxisGenerator = d3.axisBottom(scales.xScale);
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const y_value = parseFloat(parts[1]);
            if ( y_value < minY || minY===undefined) minY = y_value;
            if ( y_value > maxY || maxY===undefined) maxY = y_value;
        }
        scales.yScale = d3.scaleLinear()
               .domain([minY, maxY])
               .range([config.margin_top+config.inner_height,config.margin_top]);
        scales.yAxisGenerator = d3.axisLeft(scales.yScale);
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const x_value = scales.xScale(parts[0]) + ((Math.random()-0.5)*(Math.random()-0.5))*group_width;
            const y_value = parseFloat(parts[1]);
            positions.push( { x:x_value-config.halfSquareWidth, y:scales.yScale(y_value)-config.halfSquareHeight, g:group } );
        }
        return positions;
    }
}

class ScatterchartXCategoricalGridLayout extends ScatterchartLayout{
    constructor(){
        super();
    }
    getXOpacity(){
        return 1.0;
    }
    getYOpacity(){
        return 1.0;
    }
    getSortedGroups(items,data){
        const groups = [];
        for (let i=0;i<items;i++){
            groups.push( this.sortFunction(data[i]) );
        }
        return groups;
    }
    layoutPositions(data,config,scales){
        const items = data.length;
        const positions = [];
        const sortedGroups = this.getSortedGroups( items , data );
        let minY= undefined;
        let maxY= undefined;
        const groups = [];
        for (let i in sortedGroups){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const g = parts[0];
            if( groups.indexOf(g) == -1 ){
                groups.push(g);
            }
        }
        const group_positions = [];
        const group_width = (config.inner_width / groups.length);
        for (let i in groups){
            group_positions[i] = config.margin_left + group_width*parseFloat(i) + config.squareSpacingX;
        }
        scales.xScale = d3.scaleOrdinal()
            .domain(groups)
            .range(group_positions);
        scales.xAxisGenerator = d3.axisBottom(scales.xScale);
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const y_value = parseFloat(parts[1]);
            if ( y_value < minY || minY===undefined) minY = y_value;
            if ( y_value > maxY || maxY===undefined) maxY = y_value;
        }
        scales.yScale = d3.scaleLinear()
               .domain([minY, maxY])
               .range([config.margin_top+config.inner_height,config.margin_top + config.squareSpacingY]);
        scales.yAxisGenerator = d3.axisLeft(scales.yScale);
        const counters = {};
        for (let i=0;i<items;i++){
            const group = sortedGroups[i];
            const parts = group.split(",");
            const base_y_value = scales.yScale(parseFloat(parts[1]));
            const offset_y = base_y_value % config.squareSpacingY;
            const plotY = base_y_value - offset_y;
            const key = parts[0]+","+plotY;
            if ( counters[key] === undefined ){
                counters[key] = 0;
            }else{
                counters[key]++;
            }
            const x_offset = counters[key] * config.squareSpacingX;
            const x_value = scales.xScale(parts[0]) + x_offset;
            positions.push( { x:x_value-config.halfSquareWidth, y:plotY-config.halfSquareHeight, g:group } );
        }
        return positions;
    }
}