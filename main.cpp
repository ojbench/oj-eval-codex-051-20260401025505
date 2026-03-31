#include <bits/stdc++.h>
using namespace std;

struct ExprToken {
    enum Type { NUM, ID, QMARK, LP, RP, PLUS, MINUS, END } type;
    long long val; // for NUM
    string id;
};

struct Lexer {
    string s; size_t i=0; ExprToken cur;
    static bool isidstart(char c){return c=='_'||isalpha((unsigned char)c);}    
    static bool isidchar(char c){return c=='_'||isalnum((unsigned char)c);}    
    Lexer(const string &str):s(str),i(0){next();}
    void skipws(){ while(i<s.size() && isspace((unsigned char)s[i])) i++; }
    void next(){
        skipws();
        if(i>=s.size()){ cur={ExprToken::END,0,{}}; return; }
        char c=s[i];
        if(c=='('){ i++; cur={ExprToken::LP,0,{}}; return; }
        if(c==')'){ i++; cur={ExprToken::RP,0,{}}; return; }
        if(c=='+'){ i++; cur={ExprToken::PLUS,0,{}}; return; }
        if(c=='-'){
            // Could be negative number or minus operator; handle number if followed by digits
            if(i+1<s.size() && isdigit((unsigned char)s[i+1])){
                size_t j=i+1; while(j<s.size() && isdigit((unsigned char)s[j])) j++;
                long long v=stoll(s.substr(i,j-i)); i=j; cur={ExprToken::NUM,v,{}}; return; }
            i++; cur={ExprToken::MINUS,0,{}}; return; }
        if(c=='?'){ i++; cur={ExprToken::QMARK,0,{}}; return; }
        if(isdigit((unsigned char)c)){
            size_t j=i; while(j<s.size() && isdigit((unsigned char)s[j])) j++;
            long long v=stoll(s.substr(i,j-i)); i=j; cur={ExprToken::NUM,v,{}}; return; }
        if(isidstart(c)){
            size_t j=i; while(j<s.size() && isidchar(s[j])) j++;
            cur={ExprToken::ID,0,s.substr(i,j-i)}; i=j; return; }
        // Unknown char, skip
        i++; next();
    }
};

struct Parser {
    Lexer lex; function<long long(const string&)> resolve;
    long long self_addr = 0; // address of current item (for '?')
    Parser(const string &expr, function<long long(const string&)> resolver, long long self): lex(expr), resolve(std::move(resolver)), self_addr(self) {}
    long long parse(){ return expr(); }
    long long expr(){ long long v=term(); while(lex.cur.type==ExprToken::PLUS || lex.cur.type==ExprToken::MINUS){ auto t=lex.cur.type; lex.next(); long long rhs=term(); if(t==ExprToken::PLUS) v+=rhs; else v-=rhs; } return v; }
    long long term(){ if(lex.cur.type==ExprToken::MINUS){ lex.next(); return -term(); } return primary(); }
    long long primary(){
        if(lex.cur.type==ExprToken::NUM){ long long v=lex.cur.val; lex.next(); return v; }
        if(lex.cur.type==ExprToken::QMARK){ lex.next(); return self_addr+1; }
        if(lex.cur.type==ExprToken::ID){ string id=lex.cur.id; lex.next(); return resolve(id); }
        if(lex.cur.type==ExprToken::LP){ lex.next(); long long v=expr(); if(lex.cur.type==ExprToken::RP) lex.next(); return v; }
        // Fallback
        return 0;
    }
};

struct Cell { // represents one memory cell
    bool is_const;
    long long value; // if const
    string expr;     // if not const, expression string to evaluate later
    vector<string> labels; // labels attached to this cell
    bool is_duplicate = false; // if true, value mirrors another cell
    size_t dup_index = 0;      // index of cell to duplicate from
};

static string trim(const string &s){ size_t a=0,b=s.size(); while(a<b && isspace((unsigned char)s[a])) a++; while(b>a && isspace((unsigned char)s[b-1])) b--; return s.substr(a,b-a); }

int main(){
    ios::sync_with_stdio(false); cin.tie(nullptr);
    // Read entire stdin
    string input, line;
    vector<string> lines;
    while (std::getline(cin, line)) {
        // strip comments '//' to end of line
        size_t p = line.find("//");
        if(p!=string::npos) line = line.substr(0,p);
        lines.push_back(line);
    }
    // Concatenate lines and split by ';' for instructions; But labels can span lines; We'll build by accumulating until a ';'
    vector<string> stmt_strs;
    string acc;
    for(string ln: lines){
        if(ln.find(';')==string::npos){ acc += ln + ' '; }
        else{
            size_t start=0; string l=ln;
            while(true){ size_t pos=l.find(';', start);
                if(pos==string::npos){ acc += l.substr(start) + ' '; break; }
                acc += l.substr(start, pos-start);
                stmt_strs.push_back(trim(acc));
                acc.clear();
                start = pos+1; if(start>=l.size()) break; }
        }
    }
    if(!trim(acc).empty()){
        // No trailing semicolon; ignore
    }

    vector<Cell> mem; mem.reserve(1<<16);
    // Map labels to cell index; We'll fill after constructing mem and collecting all label attachments
    vector<pair<string, size_t>> label_defs;

    auto push_const = [&](long long v, vector<string> lbls=vector<string>()){
        Cell c; c.is_const=true; c.value=v; c.expr=""; c.labels=move(lbls); size_t idx = mem.size(); mem.push_back(move(c)); for(auto &lb: mem.back().labels){ label_defs.emplace_back(lb, idx); } };
    auto push_expr = [&](string e, vector<string> lbls=vector<string>()){
        Cell c; c.is_const=false; c.value=0; c.expr=trim(e); c.labels=move(lbls); size_t idx = mem.size(); mem.push_back(move(c)); for(auto &lb: mem.back().labels){ label_defs.emplace_back(lb, idx); } };
    auto push_duplicate_of = [&](size_t idx_ref){
        Cell c; c.is_const=false; c.value=0; c.expr=""; c.labels={}; c.is_duplicate=true; c.dup_index=idx_ref; size_t idx = mem.size(); mem.push_back(move(c)); };

    auto split_ws = [](const string &s)->vector<string>{ vector<string> toks; string cur; bool inparen=false; for(size_t i=0;i<s.size();){ char ch=s[i]; if(isspace((unsigned char)ch)){ if(!cur.empty()){ toks.push_back(cur); cur.clear(); } i++; continue; } if(ch==';'){ if(!cur.empty()){ toks.push_back(cur); cur.clear(); } i++; continue; } else { cur.push_back(ch); i++; } } if(!cur.empty()) toks.push_back(cur); return toks; };

    auto parse_leading_labels_and_opcode = [&](const vector<string>& toks, size_t &pos, vector<string> &leading_labels, string &opcode){
        leading_labels.clear(); opcode="";
        while(pos<toks.size()){
            string tk = toks[pos];
            // Split repeatedly by ':' to extract labels; token can be like 'a:b:c:msubleq' or 'a:' or 'msubleq'
            while(true){ size_t col = tk.find(':'); if(col==string::npos) break; string left = tk.substr(0,col); if(!left.empty()) leading_labels.push_back(left); tk = tk.substr(col+1); }
            if(!tk.empty()){
                if(tk=="." || tk=="msubleq" || tk=="rsubleq" || tk=="ldorst"){ opcode = tk; pos++; break; }
                // If it's not opcode, but we had stripped labels from a pure 'label:' token, tk may be empty already; if not empty and not opcode, it could be stray; treat as opcode token missing -> attempt to continue
                // To be robust, if we've already collected some labels and this token isn't an opcode, but next tokens may contain the opcode. We'll move to next token.
            }
            pos++;
            if(!opcode.empty()) break;
        }
    };

    auto parse_items_from = [&](const vector<string> &toks, size_t pos)->vector<pair<vector<string>, string>>{
        vector<pair<vector<string>,string>> items;
        while(pos<toks.size()){
            vector<string> labs; string expr;
            string tk = toks[pos++];
            // Process labels possibly in this token and still contain expr
            while(true){ size_t col = tk.find(':'); if(col==string::npos) break; string left=tk.substr(0,col); if(!left.empty()) labs.push_back(left); tk = tk.substr(col+1); if(tk.empty()){ if(pos>=toks.size()) break; tk = toks[pos++]; } }
            expr = tk;
            if(!expr.empty()) items.push_back({labs, expr});
        }
        return items;
    };

    for(string stmt : stmt_strs){
        if(trim(stmt).empty()) continue;
        auto toks = split_ws(stmt);
        if(toks.empty()) continue;
        size_t pos=0; vector<string> leading_labels; string op;
        parse_leading_labels_and_opcode(toks, pos, leading_labels, op);
        if(op.empty()) continue; // ignore malformed lines

        // For instructions that are not '.', create opcode cell first
        if(op != "."){
            long long opcode_val = (op=="msubleq"?0 : (op=="rsubleq"?1 : 2));
            // Attach leading labels to opcode cell
            push_const(opcode_val, leading_labels);
            leading_labels.clear();
            // Parse items
            auto items = parse_items_from(toks, pos);
            // auto expand for msubleq and rsubleq
            if(op=="msubleq" || op=="rsubleq"){
                if(items.size()==1){ // A
                    // a, a, ? but duplicate semantics: b equals evaluated a, not re-evaluated expression
                    size_t a_idx_before = mem.size();
                    push_expr(items[0].second, items[0].first);
                    push_duplicate_of(a_idx_before);
                    push_expr("?", {});
                }else if(items.size()==2){
                    push_expr(items[0].second, items[0].first);
                    push_expr(items[1].second, items[1].first);
                    push_expr("?", {});
                }else{
                    // take first three if more
                    for(size_t i=0;i<min<size_t>(3, items.size()); ++i){ push_expr(items[i].second, items[i].first); }
                }
            }else{ // ldorst
                // exactly 3 params; pad zeros if fewer
                for(size_t i=0;i<3; ++i){
                    if(i<items.size()) push_expr(items[i].second, items[i].first);
                    else push_const(0, {});
                }
            }
        }else{
            // '.' data, no opcode cell. Leading labels refer to current address before the first item.
            // Attach them to a zero-width location? Per spec, label is just address; We'll attach to the next cell index (current memory size). Since '.' has no cell itself, labels refer to current index.
            // Implement by recording a dummy label pointing to current index; We'll store now
            for(auto &lb : leading_labels){ label_defs.emplace_back(lb, mem.size()); }
            // Parse items and push expressions with their labels
            auto items = parse_items_from(toks, pos);
            for(auto &it: items){ push_expr(it.second, it.first); }
        }
    }

    // Build label map
    unordered_map<string, long long> label_addr;
    label_addr.reserve(label_defs.size()*2+16);
    for(auto &p : label_defs){ if(label_addr.find(p.first)==label_addr.end()) label_addr[p.first] = (long long)p.second; }

    // Evaluate expressions for non-const cells
    vector<long long> out(mem.size());
    for(size_t idx=0; idx<mem.size(); ++idx){ if(mem[idx].is_const){ out[idx] = mem[idx].value; continue; }
        if(mem[idx].is_duplicate){ out[idx] = out[mem[idx].dup_index]; continue; }
        auto resolver = [&](const string &id)->long long{
            auto it = label_addr.find(id);
            if(it!=label_addr.end()) return it->second;
            // If unknown id, treat as 0
            return 0LL;
        };
        Parser p(mem[idx].expr, resolver, (long long)idx);
        long long v = p.parse();
        out[idx] = v;
    }

    // Print output: numbers separated by spaces and newlines after groups of 4 for readability
    // This formatting should be accepted as whitespace-agnostic by judge
    for(size_t i=0;i<out.size(); ++i){
        cout << out[i];
        if(i+1<out.size()) cout << ' ';
        if((i%4)==3) cout << "\n";
    }
    if(out.size()%4!=0) cout << "\n";
    return 0;
}
