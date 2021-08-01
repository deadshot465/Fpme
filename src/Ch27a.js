const chalk = require('chalk');

exports._chalk = (styles, str) => {
    for (let i = 0, c = chalk; i < styles.length; i++) {
        c = c[styles[i]];
        return c(str);
    }
};