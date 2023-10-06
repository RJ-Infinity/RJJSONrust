use std::collections::HashMap;

fn consume_chr(string: &mut String){
	let mut chars = string.chars();
	chars.next();
	*string = chars.as_str().to_string();
}

#[derive(Debug)]
pub enum JSON{
	Null,
	Dict(HashMap<String, Box<JSON>>),
	List(Vec<Box<JSON>>),
	Bool(bool),
	String(String),
	Float(f64),
}

#[derive(Debug)]
pub enum ErrorType{
	FormatException(String),
	InvalidError,
}
static DIGITS: [char; 10] = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ];
impl JSON{
	pub fn string_to_object(mut json: String) -> Result<Self, ErrorType>
	{
		json = json.replace("\r\n", "\n").replace("\n", "");//remove all types of line ending
		let mut in_string = false;
		let mut in_esc = false;
		for ichar in 0..json.len() {//this loop removes whitespace except in strings
			let mut curr_char = json.chars().nth(ichar);
			if !in_string {
				while curr_char == Some(' ') || curr_char == Some('\t') {//while the current character is a whitespace remove it
					//while is neededd not if as otherise it fails to remove two whitespace in a row
					json = json.chars().enumerate().filter(|&(i, _)| i != ichar).map(|(_, c)|c).collect();
					curr_char = json.chars().nth(ichar);
				}
			}
			if !in_esc//stops ending strings if the " was preceded with a \
			{ if curr_char == Some('"') { in_string = !in_string; } }

			if in_esc { in_esc = false; }
			if curr_char == Some('\\') { in_esc = true; }
		}
		
		if json == "true" { return Ok(JSON::Bool(true)); }
		if json == "false" { return Ok(JSON::Bool(false)); }
		if json == "null" { return Ok(JSON::Null); }
		
		if let Some(dbl) = Self::json_parse_number(json.clone())
		{ return Ok(JSON::Float(dbl)); }
		let first_chr = json.chars().next();
		match first_chr {//get type of fist thing
			Some('"') => if json.chars().last() == Some('"') { // string
				let mut chars = json.chars();
				chars.next();
				chars.next_back();
				return Ok(JSON::String(Self::remove_esc_chars(chars.as_str().to_string())));
			}else{ return Err(ErrorType::FormatException("The JSON string is incorectly formated".to_string())); },//it is incorectly formated
			Some('{') => if json.chars().last() == Some('}') {
				let mut returnv = JSON::Dict(HashMap::new());
				if json.len() > 2 {
					let mut chars = json.chars();
					chars.next();
					chars.next_back();
					for str in Self::split_to_json_obj(chars.as_str().to_string()) {
						if let JSON::Dict(ref mut map) = returnv{
							let key_value = match Self::get_key_value_pair(str)
							{ Ok(v) => v, Err(e) => return Err(e), };
							map.insert(key_value.0, Box::new(key_value.1));
						}else{unreachable!()}
					}
				}
				return Ok(returnv);
			}else{ return Err(ErrorType::FormatException("The JSON string is incorectly formated".to_string())); },//it is incorectly formated
			Some('[') => if json.chars().last() == Some(']'){
				let mut returnv = JSON::List(Vec::new());
				if json.len() > 2 {
					let mut chars = json.chars();
					chars.next();
					chars.next_back();
					for str in Self::split_to_json_obj(chars.as_str().to_string()) {
						if let JSON::List(ref mut lst) = returnv{
							let value = Self::string_to_object(str);
							match value {
								Ok(v) => lst.push(Box::new(v)),
								Err(_) => return value,
							}
						}else{unreachable!()}
					}
				}
				return Ok(returnv);
			}else{ return Err(ErrorType::FormatException("The JSON string is incorectly formated".to_string())); },//it is incorectly formated
			_=>{return Err(ErrorType::FormatException("Json is invalid".to_string()));}//just so the compiler dosnt shout at me
		}
	}
	fn consume_digits(json: &mut String) -> String {
		let mut rv = "".to_string();

		let non_mut_json = json.clone();
		let mut chrs = non_mut_json.chars();
		let mut chr;
		while {
			chr = chrs.next();
			chr.is_some()
		} {
			if !DIGITS.iter().any(|d| d == &chr.unwrap()) { break; }
			rv.push(chr.unwrap())
		}
		json.clear();
		if chr.is_some() {
			json.push(chr.unwrap());
			json.push_str(chrs.as_str());
		}
		return rv;
	}
	pub fn json_parse_number(mut json: String) -> Option<f64>
	{
		let result = match json.parse() {
			Ok(v) => v,
			Err(_) => return None,
		};

		
		if json.starts_with('-') {consume_chr(&mut json);}
		
		let first_chr = json.chars().next();
		if json.len() == 0 || !DIGITS.iter().any(|d| d == &first_chr.unwrap()) {return None;}
		
		let first_chr = first_chr.unwrap();
		
		if first_chr != '0' { Self::consume_digits(&mut json); } else { consume_chr(&mut json) }
		
		if json.len() == 0 { return Some(result); }

		let second_char = json.chars().nth(1);
		if json.len() > 1 && json.starts_with('.') && DIGITS.iter().any(|d| d == &second_char.unwrap()) {
			consume_chr(&mut json);
			Self::consume_digits(&mut json);
		}
		if json.len() == 0 { return Some(result); }
		
		if json.starts_with('e') || json.starts_with('E') { consume_chr(&mut json); }
		else { return None;}

		if json.len() > 0 && (json.starts_with('-') || json.starts_with('+'))
		{ consume_chr(&mut json); }

		let first_chr = json.chars().next();
		if json.len() > 0 && DIGITS.iter().any(|d| d == &first_chr.unwrap())
		{ consume_chr(&mut json); }

		if json.len() == 0 { return Some(result); }

		return None;
	}
	fn get_key_value_pair(json: String) -> Result<(String, JSON), ErrorType> {
		let mut key = String::new();
		let mut value = String::new();
		let mut in_string = false;
		let mut in_esc = false;
		let mut in_key = true;
		for chr in json.chars() {
			if !in_string && chr == ':'{
				in_key = false;
				continue;
			}
			//stops ending strings if the " was preceded with a \
			if in_key && !in_esc && chr == '"' {in_string = !in_string;}
			if in_key && in_esc { in_esc = false; }
			if in_key && chr == '\\' { in_esc = true; }
			if in_key{key.push(chr)}else{value.push(chr)};
		}
		return match Self::string_to_object(value){
			Ok(val) => Ok((match {
				match Self::string_to_object(key){Ok(val) => val, Err(e) => return Err(e),}
			} {JSON::String(v)=>v, _ => return Err(ErrorType::InvalidError) }, val)),
			Err(e) => Err(e),
		};
	}
	fn remove_esc_chars(str: String) -> String {
		let mut in_esc = false;
		let mut new_str = String::new();
		for chr in str.chars() {
			if chr == '\\' && !in_esc { in_esc = true; } else if in_esc {
				in_esc = false;
				match chr {
					'b' => { new_str += "\x08"; }
					'f' => { new_str += "\x0c"; }
					'n' => { new_str += "\n"; }
					'r' => { new_str += "\r"; }
					't' => { new_str += "\t"; }
					'"' => { new_str += "\""; }
					'\\' => { new_str += "\\"; }
					_ => {
						new_str += "\\";
						new_str.push(chr);
					}
				}
			} else { new_str.push(chr); }
		}
		return new_str;
	}
	fn split_to_json_obj(mut json: String) -> Vec<String> {
		json += ",//"; // so the last element trigers the comma detection code and gets put in the list the "//" are so it dosnt run out of string
		let mut close = Vec::new(); // char
		let mut returnv = Vec::new(); // string
		let mut in_string = false;
		let mut in_esc = false;
		let mut ichar = 0;
		while ichar < json.len() {
			let curr_chr = json.chars().nth(ichar);
			if !in_string && close.len() == 0 && curr_chr == Some(',') {
				returnv.push(json.chars().take(ichar).collect());
				let mut json_chrs = json.chars();
				json_chrs.nth(ichar);
				json = json_chrs.take(json.len() - ichar - 1).collect();
				ichar = 0;
			}
			let curr_chr = json.chars().nth(ichar);
			//stops ending strings if the " was preceded with a \
			if !in_esc && curr_chr == Some('"') { in_string = !in_string; }
			
			if curr_chr == Some('{') { close.push('}'); }
			if curr_chr == Some('[') { close.push(']'); }
			
			if close.len() > 0  && curr_chr == Some(close[close.len() - 1])
			{ close.remove(close.len() - 1); }

			if in_esc { in_esc = false; }
			if curr_chr == Some('\\') { in_esc = true; }
			ichar += 1; // THIS MUST BE DONE EVERY ITTERATION PUT IT BEFORE A CONTINUE
		}
		return returnv;
	}
	pub fn object_to_string(&self) -> String {match self{
		JSON::Null => "null".to_string(),
		JSON::Dict(dct) => {
			let mut returnv = "{".to_string();
			let mut firstloop = true;
			for (key,value) in dct {
				if !firstloop { returnv += ","; }
				firstloop = false;
				returnv += &format!("\"{}\":{}", key.replace("\"", "\\\""), value.object_to_string());
			}
			returnv += "}";
			returnv
		},
		JSON::List(lst) => {
			let mut returnv = "[".to_string();
			let mut firstloop = true;
			for el in lst {
				if !firstloop { returnv += ","; }
				firstloop = false;
				returnv += &el.object_to_string();
			}
			returnv += "]";
			returnv
		},
		JSON::Bool(b) => if *b {"true".to_string()} else {"false".to_string()},
		JSON::String(s) => format!("\"{}\"",s.replace("\"", "\\\"").replace("\\", "\\\\").replace("\n", "\\n").replace("\x08", "\\b").replace("\x0c", "\\f").replace("\r", "\\r")),
		JSON::Float(f) => f.to_string(),
	}}
	pub fn format_json(json_str: String) -> String {
		let mut tabs = 0;
		let mut formated_jsonstr = String::new();
		let mut in_string = false;
		let mut esc = false;
		for chr in json_str.chars() { if esc{
			esc = false;
			formated_jsonstr += "\\";
			formated_jsonstr += &chr.to_string();
		} else if chr == '\\' { esc = true; } else if chr == '"' && !esc{
			formated_jsonstr += "\"";
			in_string = !in_string;
		}else if (chr == '[' || chr == '{') && !in_string {
			formated_jsonstr += &chr.to_string();
			formated_jsonstr += "\n";
			tabs+=1;
			for _ in 0..tabs {formated_jsonstr += "\t"; }
		} else if chr == ',' && !in_string {
			formated_jsonstr += ",\n";
			for _ in 0..tabs {formated_jsonstr += "\t"; }
		}else if (chr == ']' || chr == '}') && !in_string {
			tabs-=1;
			formated_jsonstr += "\n";
			for _ in 0..tabs {formated_jsonstr += "\t"; }
			formated_jsonstr += &chr.to_string();
		} else if chr == ':' && !in_string {formated_jsonstr += ": ";}
		else if in_string || (chr != ' ' && chr != '\t' && chr != '\n' && chr != '\r')
		{formated_jsonstr += &chr.to_string();}}
		return formated_jsonstr;
	}
}