var fs = require("fs");
var http = require("http");
var sqlite3 = require("sqlite3").verbose();
var util = require("util");
var url = require("url");
var file = "posts.db";
var exists = fs.existsSync(file);
var db = new sqlite3.Database(file);
var count = 0;

if (!exists) {
	console.log("Creating table");
	db.run("CREATE TABLE Posts (id INTEGER PRIMARY KEY, title TEXT, date TEXT, summary TEXT, body TEXT, image TEXT)");
	console.log("Table created, inserting data");
	var insert = db.prepare("INSERT INTO Posts (title, date, summary, image, body) VALUES (?, ?, ?, ?, ?)");
	insert.run("myTitle", "7-5-2015", "summary", "../images/post_headers/post_1.png", "body");
	insert.finalize();
	console.log("Data Inserted");
}

http.createServer(function (req, res) {
	console.log("Request received... ");
	var url_parts = url.parse(req.url, true);
	var query = url_parts.query;
	
    var data;
	if (query.type === "single"){
		get_post(query.postID, res, req);
	}
    else if (query.type === "count"){
        count_posts(res, req);
    }
	else if (query.type === "multi"){
		get_post_array(query.num_entries, res);
	}
    else if (query.type === "all"){
        get_all_posts(res);
    }
}).listen(8080,"127.0.0.1");
console.log("server running on port 8000");

var finalize = function(data, res, req){
        res.writeHead(200, {'Content-Type': 'application/json',
                           'Access-Control-Allow-Origin' : 'http://localhost:8000',
                           'Access-Control-Allow-Methods' : 'GET',
                            'Access-Control-Allow-Headers': 'Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With'});
        res.end(JSON.stringify(data));
}

var count_posts = function(res, req){
    db.serialize(function(){
        db.each("SELECT COUNT(id) FROM Posts", function(err, row){
            var data = {"count":row["COUNT(id)"]};
            finalize(data, res, req);
        });
    });
}

var get_post = function(entry_num, res, req){
	console.log("Retrieving post: " + entry_num);
    db.serialize(function(){
		console.log("beginning select");
		db.each("SELECT * FROM Posts WHERE id=" + entry_num, function(err, row){
			if (err)
                console.log("Error: " + err);
            console.log("Returning:");
            print_obj(row);
            finalize(row, res, req);
		});
	});
};

var get_post_array = function(num_entries, res){
	console.log("Retrieving " + num_entries + " posts");
    var data = [];
	db.serialize(function(){ 
		db.each("SELECT * FROM Posts WHERE id<=" + num_entries, function(err, row){
			if (err)
            {
                console.log("Error: " + err);
            }
			data.push(row);
		});
        print_obj(data);
        finalize({value: "still in progress"}, res);
	});
};

var get_all_posts = function(res){
    console.log("Retrieving all posts");
    var data = [];
	db.serialize(function(){ 
		db.each("SELECT * FROM Posts", function(err, row){
			if (err)
                console.log("Error: " + err);
			data.push(row);
            console.log("added row: ");
            print_obj(row);
		});
	}, function() {
        console.log("all posts: " );
        print_obj(data);
        finalize(data, res);
    });
};

var print_obj = function(object){
	console.log(util.inspect(object, {showHidden: false, depth: null}));
};