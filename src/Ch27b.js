const reverseArray = a => {
    let na = [];
    for (let i = 0; i < a.length; i++) {
        const value = a[i];
        const nValue = value.constructor.name == 'Object' ? reverseKeys(value) : value;
        na.push(nValue);
    }
    return na;
}

const reverseKeys = o => {
    const no = {};
    const entries = Object.entries(o);
    for (let i = 0; i < entries.length; i++) {
        const key = entries[i][0];
        const value = entries[i][1];
        const nValue = value.constructor.name == 'Object' ? reverseKeys(value) : (value.constructor.name == 'Array' ? reverseArray(value) : value);
        no[key.split("").reverse().join("")] = nValue;
    }
    return no;
};

exports._reverseKeys = o => JSON.stringify(reverseKeys(o))