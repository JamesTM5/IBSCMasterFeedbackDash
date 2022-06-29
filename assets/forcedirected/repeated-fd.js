class RepeatedForceDirected {
/**
 * This deals with handling multiple time series in one force directed graph
 * @param {element to render into (div)} el 
 */

    constructor(el) {
        this.el = el
        this.fd = new ForceDirected(el);
        this.buttons_div = d3.select(el).append("div").attr("class","ts-buttons");
        this.lock_communities_div = d3.select(el).append("div").attr("class","lock-communities");
        this.style_checkboxes_div = d3.select(el).append("div").attr("class","style-checkboxes");
        this.filter_div = d3.select(el).append("div").attr("class","node-filters");

        const lock_id = this.guid();

        this.lock_communities_checkbox = this.lock_communities_div.append("input").attr("type","checkbox").attr("id",lock_id);
        this.lock_communities_div.append("label").attr("for",lock_id).text("Lock Community");
        
        
        this.lock_communities_checkbox.on("click",()=>{
            const use_communities_lock = this.lock_communities_checkbox.node().checked;
            if ( use_communities_lock){
                this.fd.styleFunctions.node_community = (d)=>null;
            }else{
                this.fd.styleFunctions.node_community = (d)=>d.community;
                this.fd.redrawGraph();
            }
        });

        function mutuality_edge_colour(d){
            if (parseInt(d.mutual)==2){
                return "green";
            } else {
                return "darkgrey";
            }
        }

        this.style_controls = {
            "Centrality":(on)=>{ 
                if(on){
                    this.fd.styleFunctions.node_radius = (d)=>Math.max(1.0, parseFloat(d.Degree)*30)
                }else{ 
                    this.fd.styleFunctions.node_radius = (d)=>10.0;
                }
                this.fd.redrawGraph();
            },
            "Reciprocity":(on)=>{ 
                if(on){
                    this.fd.styleFunctions.edge_colour = mutuality_edge_colour;
                }else{ 
                    this.fd.styleFunctions.edge_colour = (d)=>"darkgrey";
                }
                this.fd.redrawGraph();
            },
            "Score":(on)=>{ 
                if(on){
                    this.fd.styleFunctions.edge_width = (d)=>parseFloat(d.score)*10;
                }else{ 
                    this.fd.styleFunctions.edge_width = (d)=>3.0;
                }
                this.fd.redrawGraph();
            }
        };

        this.setupStyleChekcboxes();
    }

    guid() {
        function _random_letter() {
            return String.fromCharCode(97+Math.floor(Math.random() * 26));
        }
        function _p8(s) {
            var p = (Math.random().toString(16)+"000000000").substr(2,8);
            return s ? "-" + p.substr(0,4) + "-" + p.substr(4,4) : _random_letter() + p.substr(0, 7);
        }
        return _p8() + _p8(true) + _p8(true) + _p8();
    }

    loadData(el, values) {
        console.log(values);
        this.values = values;
        this.ts = Object.keys(values);
        this.updateButtons();
        this.current = this.ts[0];
        this.processFilters();
        this.updateGraph();
    }

    processFilters(){
        this.node_filters = {
            id:{ ignore:true },
            degree:{ ignore:true },
            centrality:{ ignore:true },
            community:{ ignore:true }
        };
        this.node_filters_array = [];
        this.node_filter_by_id = {};

        function build_filter_function(field,value){
            return (d)=>{ 
                return d[field] == value;
            }
        }
        
        this.ts.forEach( ts_id=>{
            const data = this.values[ts_id];
            data.nodes.forEach( node =>{
                const node_fields = Object.keys(node);
                node_fields.forEach( field_name=>{
                    if ( !this.node_filters[field_name] ){
                        this.node_filters[field_name] = { title: field_name , values: [] , filters:[
                            { title:"all",  f:null, guid:this.guid(), field:field_name }
                        ] };
                        this.node_filters_array.push( this.node_filters[field_name] );
                    }
                    if ( !this.node_filters[field_name].ignore ){
                        const value = node[field_name];
                        const filter = this.node_filters[field_name];
                        if ( filter.values.indexOf(value) == -1 ){
                            filter.values.push(value);
                            filter.filters.push({
                                title:value, 
                                f:build_filter_function(field_name,value), 
                                guid:this.guid(), 
                                field:field_name
                            });
                        }
                    }
                })
            })
        });
        this.node_filters_array.forEach( filter=>{
            filter.filters.forEach( f=>{
                this.node_filter_by_id[ f.guid ] = f;
            })
        });
        
        this.updateFilters();
    }

    updateFilters(){
        const guid = this.guid()
        this.filter_div.selectAll("div").data(this.node_filters_array).join(
            (enter)=>{
                const d = enter.append("div");
                d.append("h2").text(d=>d.title);
                d.selectAll("input").data( d=>d.filters ).join(
                    enter => {
                        const el = enter.append("div").attr("class","filter-option")
                        el.append("input").attr("type","radio")
                            .attr("name",d=>guid+d.field)
                            .attr("id",d=>d.guid)
                            .property('checked', (d)=>d.title=="all")
                            .on("click", ()=>{
                                this.filtersChanged()
                            });
                        el.append("label").attr("for",d=>d.guid).text( (d)=>d.title ); 
                    },
                    update => update.text( d => d ),
                    exit=> exit.remove()
                )
            },
            (update)=>{},
            (exit)=>exit.remove()
        );
        this.filtersChanged()
    }

    filtersChanged(){
        const options = this.filter_div.selectAll("div").selectAll("div.filter-option").selectAll("input").nodes();
        const filter_functions = [];
        options.forEach( (x)=>{
            if(x.checked){
                const f = this.node_filter_by_id[x.id];
                if (f.f){
                    filter_functions.push( f.f )
                }
            }
        })
        function hilight_check(n){
            for ( let id in filter_functions ){
                if (! filter_functions[id](n) ) return false;
            }
            return true;
        }
        this.fd.styleFunctions.node_stroke = (d)=>hilight_check(d)?"black":"lightgrey";
        this.fd.redrawGraph();
    }

    updateButtons(){
        this.buttons_div.selectAll("button").data(this.ts)
            .join( (enter)=>{ enter.append("button") },
                (update)=>{},
                (remove)=>{ remove.remove() });
        this.buttons_div
            .selectAll("button")
            .text( (d)=>d )
            .on("click", (event)=>{
                this.current = event.target.innerText;
                this.updateGraph();
            });
    }

    setupStyleChekcboxes(){
        const guid = this.guid()
        this.style_checkboxes_div.selectAll("input").data(Object.keys(this.style_controls))
            .join( (enter)=>{ 
                    const el = enter.append("div").attr("class","check")
                    el.append("input").attr("type","checkbox").attr("id",d=>guid+d);
                    el.append("label").attr("for",d=>guid+d).text( (d)=>d ); 
                },
                (update)=>{},
                (remove)=>{ remove.remove() });
        this.style_checkboxes_div
            .selectAll("input")
            .attr( "data", (d)=>d )
            .on("click", (event)=>{
                const id = event.target.attributes.data.value;
                const checked = event.target.checked;
                this.style_controls[id]( checked )
            });

    }

    updateGraph(){
        this.fd.loadData(this.el , this.values[this.current])
    }
}