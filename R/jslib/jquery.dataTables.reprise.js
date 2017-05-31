function getFilterClass(i, el)
{
    if($(el).hasClass("filter-num"))  {
	return({type: "number-range"});
    } else if($(el).hasClass("filter-cat")) {
	var table = $(this).parents(".dataTable");
	var unique_values = [];
	// unfortuantely $.unique() works only on DOM elements
	table.find("td:nth-child(" + (i+1) + ")").each(function() {
		var value = $(this).text();
		if ($.inArray(value, unique_values) < 0)
		    unique_values.push(value);
	    }).get();
	return({type: "select",
		    values: unique_values});
    } else if($(el).hasClass("filter-date")) {
	return({type: "date-range"});
    } else if($(el).hasClass("filter-string")) {
	return({type: "text"});
    } else {
	return([null]);
    }  
}

//split this out into a function so that shiny can call it when it needs it.
function configureTable(i, el) {
    var filterClasses = $(this).find("th").map(getFilterClass).get();
    
    $(this).dataTable({
            "sDom": "<'row'<'span6'l><'span6'f>r>t<'row'<'span6'i><'span6'p>>",
                "sPaginationType": "bootstrap",
                "oLanguage": {
                "sLengthMenu": "_MENU_ records per page",
                    "sSearch": "Search all columns:"
                    },
                
                "aLengthMenu": [[10, 50, 100, -1], [10, 50, 100, "All"]],
                    "iDisplayLength": 10,
                    "bStateSave": true,
                    "aaSorting":[[0,'asc']],
                    "aoColumnDefs": [
                                 { "bSortable": false, "aTargets": [ "sort-off" ] },
                                 //  sort string, NA, html, scientific
                                 { "sType" : "num-robust", "aTargets": [ "sort-num-robust" ] }, 
                                 //  remove any links first, then sorts strings, NA
                                 { "sType" : "string-robust", "aTargets": [ "sort-string-robust" ] },           
                                 { "sType" : "date", "aTargets": [ "sort-date" ] }
                                     ]
                    }).columnFilter({sPlaceHolder: "head:before",
                                aoColumns : filterClasses
                                });
} 

$(document).ready(function() {
        $(".dataTable").each(configureTable);
    });

